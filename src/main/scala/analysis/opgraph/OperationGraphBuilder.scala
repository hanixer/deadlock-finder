package deadlockFinder
package analysis.opgraph

import analysis.{PredicatesPropagation, ProcessRank}
import cfg.CfgGraph
import expr.*
import lil.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class OperationGraphBuilder(func: FuncDecl):
  case class QueueEntry(label: String, node: Node, rank: Option[ProcessRank])

  type LabelAndRank = (String, Option[ProcessRank])

  private val cfg: CfgGraph = CfgGraph(func)
  private val processRanks = PredicatesPropagation.propagate(func)
  private val seen = mutable.Set.empty[LabelAndRank]
  private val edges = ListBuffer.empty[Edge]
  private val intermediates = mutable.HashMap.empty[String, IntermediateNode]
  private val queue = mutable.Queue.empty[QueueEntry]
  private val lastCallNodeForLabel = mutable.Map.empty[LabelAndRank, Node]

  def build(): OperationGraph =
    val root = new IntermediateNode(cfg.entry)
    queue += QueueEntry(cfg.entry, root, None)

    while queue.nonEmpty do processEntry(queue.dequeue())

    new OperationGraph(root, edges.toList)

  private def processEntry(entry: QueueEntry) =
    processRanks.get(entry.label) match
      case None => processRank(entry, None)
      case Some(ranks) =>
        ranks.map(r => Some(ProcessRank.Concrete(r))).foreach(processRank(entry, _))

  private def processRank(entry: QueueEntry, currRank: Option[ProcessRank]) =
    // Create new intermediate node if needed.
    val prevRank = entry.rank
    val currLabel = entry.label
    val node = entry.node
    val needIntermediate = cfg.predecessors(currLabel).lengthCompare(1) > 0
    val node1 =
      if needIntermediate then
        val node1 = intermediates.getOrElseUpdate(currLabel, new IntermediateNode(currLabel))
        edges += ((node, node1))
        node1
      else node

    // Handle statements.
    val block = func.labelToBlock(currLabel)
    val node2 = handleStmts(block, node1, currRank)

    val labelRank = (currLabel, currRank)
    if !seen(labelRank) then
      val next = nextLabels(block.transfer).map(QueueEntry(_, node2, currRank))
      queue ++= next

    seen += labelRank

  def handleStmts(block: Block, pred: Node, processRank: Option[ProcessRank]): Node =
    processRank match
      case Some(rank) =>
        def makeNode() =
          block.stmts.foldLeft(pred) { (pred, stmt) =>
            tryMakeNodeFromStmt(stmt, rank) match
              case Some(node) =>
                edges += ((pred, node))
                node
              case _ =>
                pred
          }
        val labelRank = (block.label, Some(rank))
        lastCallNodeForLabel.getOrElseUpdate(labelRank, makeNode())
      case None => pred

  def tryMakeNodeFromStmt(stmt: Stmt, processRank: ProcessRank): Option[Node] = stmt match
    case c: CallStmt                         => tryMakeNodeFromCall(c.callExpr, processRank)
    case Assignment(_, c: CallExpr, _)       => tryMakeNodeFromCall(c, processRank)
    case _                                   => None

  def tryMakeNodeFromCall(expr: CallExpr, rank: ProcessRank): Option[Node] =
    expr.args.lift(5) match
      case Some(n: IntLiteral) =>
        if expr.name == "mpi.Comm.Send" then Some(new SendNode(rank, n.n))
        else if expr.name == "mpi.Comm.Recv" then Some(new RecvNode(rank, ProcessRank.Concrete(n.n)))
        else None
      case Some(StaticFieldAccess("mpi.MPI", "ANY_SOURCE", _)) =>
        if expr.name == "mpi.Comm.Recv" then Some(new RecvNode(rank, ProcessRank.AnyRank))
        else None
      case Some(FieldAccess(_, "source", _)) =>
        if expr.name == "mpi.Comm.Send" then
          Some(new MemoSendNode(rank))
        else
          None
      case _ =>
        None

  def nextLabels(t: Transfer): List[String] = t match
    case j: Jump      => List(j.label)
    case cj: CondJump => List(cj.thenLabel, cj.elseLabel)
    case _            => List()

object OperationGraphBuilder:
  def apply(func: FuncDecl): OperationGraph =
    val result = new OperationGraphBuilder(func).build()
    InsertAdditionalNodes(result)
