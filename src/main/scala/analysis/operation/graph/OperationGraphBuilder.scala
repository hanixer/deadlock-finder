package deadlockFinder
package analysis.operation.graph

import analysis.{PredicatesPropagation, ProcessRank}
import cfg.CfgGraph
import hil.{CallExpr, IntLiteral}
import lil.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class OperationGraphBuilder(func: FuncDecl):
  type Edges = List[(Node, Node)]
  case class QueueEntry(label: String, node: Node, rank: Option[ProcessRank])
  val cfg: CfgGraph = CfgGraph(func)
  val processRanks: Map[String, ProcessRank] = PredicatesPropagation.propagate(func)

  def build(): OperationGraph =
    val root = new IntermediateNode(cfg.entry)
    val entry = QueueEntry(cfg.entry, root, None)
    val edges = loop(Queue(entry), Map(), Set(), List()).toSet.toList
    val initialMap = edges.flatMap(e => List(e._1, e._2)).map((_, List())).toMap
    val adjMap = initialMap ++ edges.groupMap(_._1)(_._2)

    new OperationGraph(root, adjMap)

  @tailrec
  private def loop(
      queue: Queue[QueueEntry],
      intermediates: Map[String, IntermediateNode],
      seen: Set[String],
      acc: Edges
  ): Edges =
    queue.dequeueOption match
      case Some((entry, rest)) =>
        val currRank = processRanks.get(entry.label)
        val isRankChanged = entry.rank.isDefined && entry.rank != currRank && entry.rank.get.isInstanceOf[ProcessRank.Concrete]

        // Create new intermediate node if needed.
        val (pred1, intermediates1) =
          if isRankChanged then
            if !intermediates.contains(entry.label) then
              val node = new IntermediateNode(entry.label)
              (node, intermediates.updated(entry.label, node))
            else (intermediates(entry.label), intermediates)
          else (entry.node, intermediates)

        val edgesPrev =
          if isRankChanged then List((entry.node, pred1))
          else List()

        val block = func.labelToBlock(entry.label)

        // Handle statements if block was not processed.
        val (edgesStmts, pred2) =
          if !seen(entry.label) then handleStmts(block.stmts, pred1, entry.label)
          else (List(), pred1)

        val next = nextLabels(block.transfer).map { QueueEntry(_, pred2, currRank) }

        loop(rest ++ next, intermediates1, seen + entry.label, edgesPrev ++ edgesStmts ++ acc)

      case None => acc

  def handleStmts(stmts: List[Stmt], pred: Node, label: String): (Edges, Node) =
    @tailrec
    def loop(stmts: List[Stmt], pred: Node, edges: Edges): (Edges, Node) =
      if stmts.isEmpty then (edges, pred)
      else
        tryMakeNodeFromStmt(stmts.head, label) match
          case Some(node) =>
            loop(stmts.tail, node, (pred, node) :: edges)
          case _ =>
            loop(stmts.tail, pred, edges)

    loop(stmts, pred, List())

  def tryMakeNodeFromStmt(stmt: Stmt, label: String): Option[Node] = stmt match
    case c: CallStmt                         => tryMakeNodeFromCall(c.callExpr, label)
    case VarDecl(_, _, Some(c: CallExpr), _) => tryMakeNodeFromCall(c, label)
    case Assignment(_, c: CallExpr, _)       => tryMakeNodeFromCall(c, label)
    case _                                   => None

  def tryMakeNodeFromCall(expr: CallExpr, label: String): Option[Node] =
    // Try to get sender rank associated with block.
    processRanks.get(label) match
      case Some(rank) =>
        // Check function name.
        if expr.name == "mpi.Comm.Send" then
          // Try to get int argument.
          expr.args.lift(5) match
            case Some(n: IntLiteral) =>
              Some(new SendNode(rank, n.n))
            case _ => None
        // TODO: refactor
        else if expr.name == "mpi.Comm.Recv" then
          // Try to get int argument.
          expr.args.lift(5) match
            case Some(n: IntLiteral) =>
              Some(new RecvNode(rank, n.n))
            case _ => None
        else None
      case _ => None

  def nextLabels(t: Transfer): List[String] = t match
    case j: Jump      => List(j.label)
    case cj: CondJump => List(cj.thenLabel, cj.elseLabel)
    case _            => List()

object OperationGraphBuilder:
  type Edges = List[(Node, Node)]

  def apply(func: FuncDecl): OperationGraph =
    new OperationGraphBuilder(func).build()
