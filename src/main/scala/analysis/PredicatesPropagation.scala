package deadlockFinder
package analysis

import hil.{AbstractVar, BinaryExpr, CallExpr, Expr, IntLiteral, UnaryExpr}
import common.{BinaryOp, UnaryOp}
import lil.{Assert, Block, FuncDecl, SsaVariable}

import cfg.{CfgGraph, Dominators}

object PredicatesPropagation:
  def propagate(func: FuncDecl): Map[String, Set[ProcessPredicate]] =
    val usesAndDefs = UsesAndDefs(func)
    val cfg = CfgGraph(func)
    val dominators = Dominators(cfg)

    func.body
      .flatMap { block => propagateFromBlock(block, usesAndDefs, dominators) }
      .groupMapReduce(_._1)(_._2.toSet)((s1, s2) => s1.union(s2))

  def propagateFromBlock(
      block: Block,
      usesAndDefs: UsesAndDefs,
      dominators: Dominators
  ): List[(String, List[ProcessPredicate])] =
    val predicates = extractPredicates(block, usesAndDefs)
    val dominated = dominators.getDominatedNodes(block.label)
    val head = (block.label, predicates)
    val tail = dominated.map(other => (other, predicates)).toList
    head :: tail

  def extractPredicates(
      block: Block,
      usesAndDefs: UsesAndDefs
  ): List[ProcessPredicate] =
    block.stmts
      .filter(_.isInstanceOf[Assert])
      .flatMap(a => evaluate(a.asInstanceOf[Assert].expr, usesAndDefs))

  def evaluate(expr: Expr, usesAndDefs: UsesAndDefs): Option[ProcessPredicate] =
    expr match
      case UnaryExpr(UnaryOp.Not, v: SsaVariable, _) =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap { defExpr =>
            evaluate(defExpr, usesAndDefs)
              .map { predicate => predicate.copy(equal = !predicate.equal) }
          }

      case v: AbstractVar =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap(evaluate(_, usesAndDefs))

      case BinaryExpr(BinaryOp.Equals, lhs, n: IntLiteral, _) =>
        if isRankCall(lhs, usesAndDefs) then
          Some(ProcessPredicate(true, n.n))
        else None

      case _ => None

  /** Returns true if a variable contains value of Rank() call or it is a Rank()
    * call expression.
    */
  def isRankCall(expr: Expr, usesAndDefs: UsesAndDefs): Boolean = expr match
    case v: SsaVariable =>
      usesAndDefs
        .getDefiningExpr(v)
        .exists(isRankCall(_, usesAndDefs))
    case call: CallExpr => call.name == "mpi.Comm.Rank"
    case _              => false
