package deadlockFinder
package analysis.operation.graph

import analysis.PredicatesPropagation
import cfg.CfgGraph
import hil.{CallExpr, IntLiteral}
import lil.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object OperationGraphBuilder:
  type Edges = List[(Node, Node)]

  def apply(func: FuncDecl): OperationGraph =
    val cfg = CfgGraph(func)
    val processRanks = PredicatesPropagation.propagate(func)

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
          else
            None
        case _ => None

    def tryMakeNodeFromStmt(stmt: Stmt, label: String): Option[Node] = stmt match
      case c: CallStmt => tryMakeNodeFromCall(c.callExpr, label)
      case VarDecl(_, _, Some(c: CallExpr), _) => tryMakeNodeFromCall(c, label)
      case Assignment(_, c: CallExpr, _) => tryMakeNodeFromCall(c, label)
      case _ => None

    def handleStmts(stmts: List[Stmt], pred: Node, label: String): (Edges, Node) =
      @tailrec
      def loop(stmts: List[Stmt], pred: Node, edges: Edges): (Edges, Node) =
        if stmts.isEmpty then
          (edges, pred)
        else
          tryMakeNodeFromStmt(stmts.head, label) match
            case Some(node) =>
              loop(stmts.tail, node, (pred, node) :: edges)
            case _ =>
              loop(stmts.tail, pred, edges)

      loop(stmts, pred, List())

    @tailrec
    def loop(queue: Queue[(String, Node)], acc: Edges): Edges =
      if queue.isEmpty then
        acc
      else
        val ((label, pred1), queue1) = queue.dequeue
        val block = func.labelToBlock(label)
        val (edges, pred2) = handleStmts(block.stmts, pred1, label)
        val next = nextLabels(block.transfer).map { (_, pred2) }
        val queue2 = queue1.enqueueAll(next)
        loop(queue2, acc.prependedAll(edges))

    val node = new Intermediate()
    val queue = Queue((cfg.entry, node))
    val edges = loop(queue, List())
    ???

  def nextLabels(t: Transfer): List[String] = t match
    case j: Jump => List(j.label)
    case cj: CondJump => List(cj.thenLabel, cj.elseLabel)
    case _ => List()

end OperationGraphBuilder
