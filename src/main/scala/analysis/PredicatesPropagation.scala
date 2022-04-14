package deadlockFinder
package analysis

import expr.*
import lil.{Assert, Block, FuncDecl, SsaVariable}

import cfg.{CfgGraph, Dominators}

/** Propagation information about process rank through basic blocks */
object PredicatesPropagation:
  case class Predicate(rank: Int, isEqual: Boolean):
    def negate: Predicate = copy(isEqual = !isEqual)

  /** Returns a map from block name to process rank. For some block there may be no entry.
    */
  def propagate(func: FuncDecl, processCount: Int = 5): Map[String, List[Int]] =
    val usesAndDefs = UsesAndDefs(func)
    val cfg = CfgGraph(func)
    val dominators = Dominators(cfg)

    func.body
      .flatMap(propagateFromBlock(_, usesAndDefs, dominators))
      .groupMap(_._1)(_._2)
      .map((label, predicates) => (label, ranksFromPredicates(predicates, processCount)))

  def ranksFromPredicates(predicates: List[Predicate], processCount: Int): List[Int] =
    if predicates.isEmpty then List.empty
    else
      predicates.find(_.isEqual) match
        case Some(predicate) => List(predicate.rank)
        case None =>
          Seq.range(0, processCount)
            .filterNot { rank => predicates.exists(_.rank == rank) }
            .toList

  /** Find predicates from asserts in the current block and propagate them to dominated blocks.
    */
  def propagateFromBlock(
      block: Block,
      usesAndDefs: UsesAndDefs,
      dominators: Dominators
  ): List[(String, Predicate)] =
    extractPredicate(block, usesAndDefs).flatMap { predicate =>
      val dominated = dominators.getDominatedNodes(block.label)
      val head = (block.label, predicate)
      val tail = dominated.map(other => (other, predicate)).toList
      head :: tail
    }

  def extractPredicate(
      block: Block,
      usesAndDefs: UsesAndDefs
  ): List[Predicate] =
    block.stmts
      .collect { case a: Assert => a }
      .flatMap { a => evaluate(a.expr, usesAndDefs) }

  /** Evaluate an expression and try to get a predicate from it. */
  def evaluate(expr: Expr, usesAndDefs: UsesAndDefs): Option[Predicate] =
    expr match
      case UnaryExpr(UnaryOp.Not, v: SsaVariable, _) =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap(evaluate(_, usesAndDefs))
          .map(_.negate)

      case v: AbstractVar =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap(evaluate(_, usesAndDefs))

      // For now, we handle only literals, without using constant propagation.
      case BinaryExpr(BinaryOp.Equals, lhs, n: IntLiteral, _) if isRankCall(lhs, usesAndDefs) =>
        Some(Predicate(n.n, true))

      case _ => None

  /** Returns true if a variable contains value of Rank() call or it is a Rank() call expression.
    */
  def isRankCall(expr: Expr, usesAndDefs: UsesAndDefs): Boolean = expr match
    case v: SsaVariable =>
      usesAndDefs
        .getDefiningExpr(v)
        .exists(isRankCall(_, usesAndDefs))
    case call: CallExpr => call.name == "mpi.Comm.Rank"
    case _              => false
