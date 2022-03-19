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
  private val callNodes = mutable.HashMap.empty[CallExpr, CallNode]
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

    // Handle statements.
    val pred2 = handleStmts(block.stmts, pred1, currRank)

    if !seen(entry.label) then
      val next = nextLabels(block.transfer).map { QueueEntry(_, pred2, currRank) }
      queue ++= next

    seen += entry.label

  private def createIntermediateIfNeeded(entry: QueueEntry, isRankChanged: Boolean) =
    if isRankChanged then
      if !intermediates.contains(entry.label) then
        val node = new IntermediateNode(entry.label)
        intermediates.put(entry.label, node)
        node
      else intermediates(entry.label)
    else entry.node

  def handleStmts(stmts: List[Stmt], pred: Node, processRank: Option[ProcessRank]): Node =
    stmts.foldLeft(pred) { (pred, stmt) =>
      tryMakeNodeFromStmt(stmt, processRank) match
        case Some(node) =>
          edges += ((pred, node))
          node
        case _ =>
          pred
    }

  def tryMakeNodeFromStmt(stmt: Stmt, processRank: Option[ProcessRank]): Option[Node] = stmt match
    case c: CallStmt                         => tryMakeNodeFromCall(c.callExpr, processRank)
    case VarDecl(_, _, Some(c: CallExpr), _) => tryMakeNodeFromCall(c, processRank)
    case Assignment(_, c: CallExpr, _)       => tryMakeNodeFromCall(c, processRank)
    case _                                   => None

  def tryMakeNodeFromCall(expr: CallExpr, processRank: Option[ProcessRank]): Option[Node] =
    def update(node: CallNode): Option[CallNode] =
      callNodes.put(expr, node)
      Some(node)

    val existing = callNodes.get(expr)
    if existing.isDefined then existing
    else
      processRank match
        case Some(rank) =>
          expr.args.lift(5) match
            case Some(n: IntLiteral) =>
              if expr.name == "mpi.Comm.Send" then
                update(new SendNode(rank, n.n))
              else if expr.name == "mpi.Comm.Recv" then
                update(new RecvNode(rank, n.n))
              else None
            case _ => None
        case None => None

  def nextLabels(t: Transfer): List[String] = t match
    case j: Jump      => List(j.label)
    case cj: CondJump => List(cj.thenLabel, cj.elseLabel)
    case _            => List()

object OperationGraphBuilder:
  def apply(func: FuncDecl): OperationGraph =
    new OperationGraphBuilder(func).build()
