package deadlockFinder
package analysis

import expr.*
import lil.{Assert, Block, FuncDecl, SsaVariable}

import cfg.{CfgGraph, Dominators}

/** Propagation information about process rank through basic blocks */
object PredicatesPropagation:
  /** Returns a map from block name to process rank.
   * For some block there may be no entry. */
  def propagate(func: FuncDecl): Map[String, ProcessRank] =
    val usesAndDefs = UsesAndDefs(func)
    val cfg = CfgGraph(func)
    val dominators = Dominators(cfg)

    func.body
      .flatMap { block => propagateFromBlock(block, usesAndDefs, dominators) }
      .toMap

  /** Find predicates from asserts in the current block and propagate them
   *  to dominated blocks. */
  def propagateFromBlock(
      block: Block,
      usesAndDefs: UsesAndDefs,
      dominators: Dominators
  ): List[(String, ProcessRank)] = 
    extractPredicate(block, usesAndDefs) match
      case None => List()
      case Some(predicate) =>
        val dominated = dominators.getDominatedNodes(block.label)
        val head = (block.label, predicate)
        val tail = dominated.map(other => (other, predicate)).toList
        head :: tail

  def extractPredicate(
      block: Block,
      usesAndDefs: UsesAndDefs
  ): Option[ProcessRank] =
    val evaluatedAsserts = block.stmts
      .filter(_.isInstanceOf[Assert])
      .flatMap(a => evaluate(a.asInstanceOf[Assert].expr, usesAndDefs))
    
    // First, try to find concrete rank.
    // Second, try to find any rank.
    val concrete = evaluatedAsserts.find(rank => rank.isInstanceOf[ProcessRank.Concrete])
    
    if concrete.isDefined then
      concrete
    else
      evaluatedAsserts.find(rank => rank match
        case ProcessRank.AnyRank => true
        case _ => false)

  /** Evaluate an expression and try to get predicates from it. */
  def evaluate(expr: Expr, usesAndDefs: UsesAndDefs): Option[ProcessRank] =
    expr match
      case UnaryExpr(UnaryOp.Not, v: SsaVariable, _) =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap { defExpr =>
            evaluate(defExpr, usesAndDefs)
              .map { _ => ProcessRank.AnyRank }
          }

      case v: AbstractVar =>
        usesAndDefs
          .getDefiningExpr(v)
          .flatMap(evaluate(_, usesAndDefs))

      // So far, we handle only literals, without using constant propagation.
      case BinaryExpr(BinaryOp.Equals, lhs, n: IntLiteral, _) if isRankCall(lhs, usesAndDefs) =>
        Some(ProcessRank.Concrete(n.n))

      case BinaryExpr(BinaryOp.Equals, lhs, _, _) if isRankCall(lhs, usesAndDefs) =>
        Some(ProcessRank.AnyRank)

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
