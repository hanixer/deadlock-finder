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
  case class QueueEntry(label: String, node: Node)

  type LabelAndRank = (String, Option[ProcessRank])

  private val cfg: CfgGraph = CfgGraph(func)
  private val processRanks = PredicatesPropagation.propagate(func)
  private val seen = mutable.Set.empty[String]
  private val edges = ListBuffer.empty[Edge]
  private val intermediates = mutable.HashMap.empty[String, IntermediateNode]
  private val queue = mutable.Queue.empty[QueueEntry]
  private val lastCallNodeForLabel = mutable.Map.empty[LabelAndRank, Node]

  def build(): OperationGraph =
    val root = new IntermediateNode(cfg.entry)
    queue += QueueEntry(cfg.entry, root)

    while queue.nonEmpty do processEntry(queue.dequeue())

    new OperationGraph(root, edges.toList)

  private def processEntry(entry: QueueEntry) =
    val ranks = processRanks.getOrElse(entry.label, List.empty)
    val currLabel = entry.label
    val node = entry.node
    val needIntermediate = cfg.predecessors(currLabel).lengthCompare(1) > 0
    val node1 =
      if needIntermediate then
        val node1 = intermediates.getOrElseUpdate(currLabel, new IntermediateNode(currLabel))
        edges += ((node, node1))
        node1
      else node

    val block = func.labelToBlock(currLabel)
    if ranks.isEmpty then
      queueNext(currLabel, block, node1, false)
    else
      // Handle statements for each rank.
      for rank <- ranks do
        val (node2, callNodeCreated) = handleStmts(block, node1, ProcessRank.Concrete(rank))
        queueNext(currLabel, block, node2, callNodeCreated)

  private def queueNext(currLabel: String, block: Block, node: Node, callNodeCreated: Boolean) =
    if seen.add(currLabel) || callNodeCreated then
      val next = nextLabels(block.transfer).map(QueueEntry(_, node))
      queue ++= next

  /** If (label, rank) combination was not seen yet, try to create call nodes from statements
    * of the given block.
    * Returns a node and a boolean flag. If the flag is true then call nodes were created during
    * the current call, otherwise false.*/
  def handleStmts(block: Block, pred: Node, rank: ProcessRank): (Node, Boolean) =
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
    lastCallNodeForLabel.get(labelRank) match
      case None =>
        val node = makeNode()
        lastCallNodeForLabel.put(labelRank, node)
        (node, node != pred)
      case Some(node) => (node, false)

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
