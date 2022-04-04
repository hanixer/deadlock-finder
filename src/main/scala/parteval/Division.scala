package deadlockFinder
package parteval

import cfg.CfgGraph
import lil.*
import hil.{ArrayCreation, BinaryExpr, CallExpr, Expr, IntLiteral, UnaryExpr, Variable}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Divides variables in function declaration into dynamic and static. */
class Division(func: FuncDecl):
  private val cfg = CfgGraph(func)

  val dynamics: Set[String] = compute()

  def isDynamic(variable: String): Boolean = dynamics.contains(variable)

  /** Return a set of dynamic variables. */
  def compute(): Set[String] =
    @tailrec
    def loop(queue: Queue[String], dynamics: Set[String], seen: Set[String]): Set[String] =
      queue.dequeueOption match
        case Some(label, rest) =>
          val block = func.labelToBlock(label)
          val newDynamics = handleBlock(block, dynamics)
          if !seen.contains(label) then
            val successors = cfg.successors(label).filterNot(seen)
            loop(rest ++ successors, newDynamics, seen + label)
          else if dynamics.subsetOf(newDynamics) then
            val successors = cfg.successors(label)
            loop(rest ++ successors, newDynamics, seen)
          else
            loop(rest, dynamics, seen)
        case None => dynamics

    val initial = func.params.map(_.name).toSet
    loop(Queue(cfg.entry), initial, Set.empty)

  def handleBlock(block: Block, dynamics: Set[String]): Set[String] =
    block.stmts.foldLeft(dynamics) { (dynamics, stmt) => stmt match
      case a: Assignment =>
        if isDynamic(a.rhs, dynamics) then
          dynamics + a.lhs.name
        else dynamics
      case v: VarDecl =>
        if v.rhs.isDefined && isDynamic(v.rhs.get, dynamics) then
          dynamics + v.v.name
        else dynamics
      case _ => dynamics
    }

  private def isDynamic(expr: Expr, dynamics: Set[String]): Boolean =
    expr match
      case _: IntLiteral => false
      case v: Variable => dynamics.contains(v.name)
      case u: UnaryExpr => isDynamic(u.e, dynamics)
      case b: BinaryExpr => isDynamic(b.lhs, dynamics) || isDynamic(b.rhs, dynamics)
      case _: CallExpr => true
      case _: ArrayCreation => true
      case _ => true
