package deadlockFinder
package analysis.opgraph

import analysis.{PredicatesPropagation, ProcessRank}
import cfg.CfgGraph
import hil.{CallExpr, IntLiteral}
import lil.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class OperationGraphBuilder(func: FuncDecl):
  case class QueueEntry(label: String, node: Node, rank: Option[ProcessRank])

  val cfg: CfgGraph = CfgGraph(func)
  val processRanks: Map[String, ProcessRank] = PredicatesPropagation.propagate(func)
  private val seen = mutable.Set.empty[String]
  private val edges = ListBuffer.empty[(Node, Node)]
  private val intermediates = mutable.HashMap.empty[String, IntermediateNode]
  private val queue = mutable.Queue.empty[QueueEntry]

  def build(): OperationGraph =
    val root = new IntermediateNode(cfg.entry)
    queue += QueueEntry(cfg.entry, root, None)

    while queue.nonEmpty do processEntry(queue.dequeue())

    val initialMap = edges.flatMap(e => List(e._1, e._2)).map((_, List())).toMap
    val adjMap = initialMap ++ edges.toList.groupMap(_._1)(_._2)

    new OperationGraph(root, adjMap)

  private def processEntry(entry: QueueEntry) =
    val currRank = processRanks.get(entry.label)
    val isRankChanged =
      entry.rank.isDefined && entry.rank != currRank && entry.rank.get.isInstanceOf[ProcessRank.Concrete]

    // Create new intermediate node if needed.
    val pred1 = createIntermediateIfNeeded(entry, isRankChanged)

    if isRankChanged then edges += ((entry.node, pred1))

    val block = func.labelToBlock(entry.label)

    // Handle statements if block was not processed.
    val pred2 =
      if !seen(entry.label) then handleStmts(block.stmts, pred1, entry.label)
      else pred1

    val next = nextLabels(block.transfer).map {
      QueueEntry(_, pred2, currRank)
    }

    if !seen(entry.label) then queue ++= next

    seen += entry.label

  private def createIntermediateIfNeeded(entry: QueueEntry, isRankChanged: Boolean) =
    if isRankChanged then
      if !intermediates.contains(entry.label) then
        val node = new IntermediateNode(entry.label)
        intermediates.put(entry.label, node)
        node
      else intermediates(entry.label)
    else entry.node

  def handleStmts(stmts: List[Stmt], pred: Node, label: String): Node =
    stmts.foldLeft(pred) { (pred, stmt) =>
      tryMakeNodeFromStmt(stmt, label) match
        case Some(node) =>
          edges += ((pred, node))
          node
        case _ =>
          pred
    }

  def tryMakeNodeFromStmt(stmt: Stmt, label: String): Option[Node] = stmt match
    case c: CallStmt                         => tryMakeNodeFromCall(c.callExpr, label)
    case VarDecl(_, _, Some(c: CallExpr), _) => tryMakeNodeFromCall(c, label)
    case Assignment(_, c: CallExpr, _)       => tryMakeNodeFromCall(c, label)
    case _                                   => None

  def tryMakeNodeFromCall(expr: CallExpr, label: String): Option[Node] =
    processRanks.get(label) match
      case Some(rank) =>
        expr.args.lift(5) match
          case Some(n: IntLiteral) =>
            if expr.name == "mpi.Comm.Send" then Some(new SendNode(rank, n.n))
            else if expr.name == "mpi.Comm.Recv" then Some(new RecvNode(rank, n.n))
            else None
          case _ => None
      case None => None

  def nextLabels(t: Transfer): List[String] = t match
    case j: Jump      => List(j.label)
    case cj: CondJump => List(cj.thenLabel, cj.elseLabel)
    case _            => List()

object OperationGraphBuilder:
  type Edges = List[(Node, Node)]

  def apply(func: FuncDecl): OperationGraph =
    new OperationGraphBuilder(func).build()
