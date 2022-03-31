package deadlockFinder
package analysis

import cfg.CfgGraph
import hil.{ArrayCreation, BinaryExpr, CallExpr, Expr, UnaryExpr, Variable}
import lil.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Computes liveness information for variables in blocks of function. */
class Liveness(func: FuncDecl):
  private val cfg = CfgGraph(func)
  private val labelToLiveVars = compute()

  /** Returns true if variable is live in block with given label */
  def isVariableLiveInBlock(variable: String, label: String): Boolean =
    labelToLiveVars.get(label) match
      case Some(vars) => vars.contains(variable)
      case _ => false

  def compute(): Map[String, Set[String]] =
    @tailrec
    def loop(queue: Queue[String], liveMap: Map[String, Set[String]]): Map[String, Set[String]] =
      queue.dequeueOption match
        case Some((label, rest)) =>
          val usedNext = cfg.successors(label).toSet.flatMap { s => liveMap.getOrElse(s, Set.empty) }
          val block = func.labelToBlock(label)
          val newLive = handleBlock(block, usedNext)
          val predecessors = cfg.predecessors(label)
          liveMap.get(label) match
            case Some(live) if live != newLive =>
              loop(rest ++ predecessors, liveMap.updated(label, newLive))
            case Some(_) =>
              loop(rest, liveMap)
            case None =>
              loop(rest ++ predecessors, liveMap.updated(label, Set.empty))
        case _ => liveMap

    loop(Queue(cfg.exit), Map.empty)

  def handleBlock(block: Block, usedNext: Set[String]): Set[String] =
    @tailrec
    def loop(stmts: List[Stmt], usedNext: Set[String]): Set[String] =
      stmts match
        case stmt :: rest =>
          val defined = definedVariable(stmt)
          val used = usedVariables(stmt)
          loop(rest, usedNext -- defined ++ used)
        case Nil => usedNext

    loop(block.transfer +: block.stmts.reverse, usedNext)

  def definedVariable(stmt: Stmt): Option[String] = stmt match
    case a: Assignment => Some(a.lhs.name)
    case v: VarDecl => Some(v.v.name)
    case _ => None

  def usedVariables(stmt: Stmt): List[String] = stmt match
    case a: Assignment => usedVariables(a.rhs)
    case v: VarDecl => v.rhs.map(usedVariables).getOrElse(Nil)
    case a: Assert => usedVariables(a.expr)
    case c: CallStmt => usedVariables(c.callExpr)
    case c: CondJump => usedVariables(c.cond)
    case r: Return => r.expr.map(usedVariables).getOrElse(Nil)
    case _ => List()

  def usedVariables(expr: Expr): List[String] = expr match
    case b: BinaryExpr => usedVariables(b.lhs) ++ usedVariables(b.rhs)
    case u: UnaryExpr => usedVariables(u.e)
    case c: CallExpr => c.args.flatMap(usedVariables)
    case a: ArrayCreation => usedVariables(a.sizeExpr)
    case v: Variable => List(v.name)
    case _ => List()

