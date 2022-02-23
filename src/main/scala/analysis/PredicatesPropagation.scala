package deadlockFinder
package analysis

import hil.{AbstractVar, BinaryExpr, CallExpr, Expr, IntLiteral, UnaryExpr}
import common.{BinaryOp, UnaryOp}
import lil.{FuncDecl, SsaVariable}

// We need a way to extract predicates from expressions.
// What data do we need to extract a predicate?
// We need a map: variable -> Option[defining expression]
// Example:
//   var p = get process
//   var b1 = p == 0
//   var b2 = !b1
// 1. We get b2 variable.
// 2. Find defining expression of b2
// 3. It is !b1. We need to resolve b1 and return to this point.
// 4. Resolving b1 variable. Its' defining expression is p == 0.
// 5. Can we resolve p also? We can, it is get process.
// 6. Substitute p and get "get process" == 0.
// 7. b1 is fully evaluated.
// 8. Now we return to 3. and negated resulting expression.
// 9. The final result is "get process" != 0.

class PredicatesPropagation:
  def propagate(func: FuncDecl): Map[String, Set[ProcessPredicate]] =
    // we can add predicates to that map
    // map: block -> set of predicates.
    // for each block
    //   blocks = get all dominated blocks
    //
    for (b <- func.body) do
      println(b.label)
    val usesAndDefs = UsesAndDefs(func)
        
    ???

  def evaluate(expr: Expr, usesAndDefs: UsesAndDefs): Option[ProcessPredicate] = expr match
    case UnaryExpr(UnaryOp.Not, v: SsaVariable, _) =>
      usesAndDefs.getDefiningExpr(v)
        .flatMap { defExpr =>
          evaluate(defExpr, usesAndDefs)
            .map { predicate => predicate.copy(equal = !predicate.equal) }
        }

    case v: SsaVariable =>
      usesAndDefs.getDefiningExpr(v)
        .flatMap { defExpr =>
          // Convert expression to process predicate.
          defExpr match
            case BinaryExpr(BinaryOp.Equals, lhs, n: IntLiteral, _) =>
              if isRankCall(lhs, usesAndDefs) then
                Some(ProcessPredicate(true, n.n))
              else None
            case _ => None
        }

    case _ => None

  /** Returns true if a variable contains value of Rank() call or it is a Rank() call expression. */
  def isRankCall(expr: Expr, usesAndDefs: UsesAndDefs): Boolean = expr match
    case v: SsaVariable =>
      usesAndDefs.getDefiningExpr(v)
        .exists(isRankCall(_, usesAndDefs))
    case call: CallExpr => call.name == "mpi.Comm.Rank"
    case _ => false
    