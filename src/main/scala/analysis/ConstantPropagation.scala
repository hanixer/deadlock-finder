package deadlockFinder
package analysis

import common.*
import hir.{Assignment, Block, FuncDecl, Stmt, VarDecl, *}
import lil.*

object ConstantPropagation:
  /** Constant abstract value. */
  enum ConstantAbsVal:
    case Undefined
    case Constant(n: Int)
    case NotConstant

  case class VarInfo(name: String, index: Int)
  object VarInfo:
    def apply(v: AbstractVar): VarInfo = v match
      case SsaVariable(n, i, _) => VarInfo(n, i)
      case _ => throw new Error("Only SSA variable is expected")

  type ConstantsMap = Map[VarInfo, ConstantAbsVal]

  def evalBinop(op: BinaryOp, n1: Int, n2: Int): ConstantAbsVal = op match
    case BinaryOp.Plus   => ConstantAbsVal.Constant(n1 + n2)
    case BinaryOp.Minus  => ConstantAbsVal.Constant(n1 - n2)
    case BinaryOp.Times  => ConstantAbsVal.Constant(n1 * n2)
    case BinaryOp.Divide => ConstantAbsVal.Constant(n1 / n2)
    case _               => ConstantAbsVal.NotConstant

  /** Collect immediate constants and add to map. Other variables marked undefined. */
  def getImmediateConstants(func: FuncDecl): ConstantsMap =
    def visitSimpleExpr(expr: SimpleExpr): ConstantAbsVal = expr match
      case i: IntLiteral => ConstantAbsVal.Constant(i.n)
      case _             => ConstantAbsVal.Undefined

    def visitExpr(expr: Expr): ConstantAbsVal = expr match
      case BinaryExpr(op, IntLiteral(n1, _), IntLiteral(n2, _), _) =>
        evalBinop(op, n1, n2)
      case se: SimpleExpr => visitSimpleExpr(se)
      case _ => ConstantAbsVal.Undefined

    def visitStmt(stmt: Stmt): Option[(VarInfo, ConstantAbsVal)] = stmt match
      case vd: VarDecl =>
        val vi = VarInfo(vd.v)
        vd.rhs
          .map(visitExpr)
          .map(cv => (vi, cv))
          .orElse(Some(vi, ConstantAbsVal.Undefined))
      case a: Assignment =>
        val vi = VarInfo(a.lhs)
        Some(vi, visitExpr(a.rhs))
      case _ => None

    def visitBlock(block: Block): List[(VarInfo, ConstantAbsVal)] =
      val paramsM = block.params.flatMap(p => Some(VarInfo(p.v), ConstantAbsVal.Undefined))
      val stmtsM = block.stmts.flatMap(visitStmt)
      paramsM ++ stmtsM

    func.body.flatMap(visitBlock).toMap

  // 1. Run all expressions, evaluate immediate constant expression,
  //    i.e. those containing only immediate constants.
  // 2. All other variables are initialized to top (undefined).
  // 3. Add to worklist all SSA edges from a constant definition to uses.
  // 4. If worklist is empty - end algorithm.
  // 5. Take item from worklist.
  // 6. Take destination and evaluate it.
  // 7. If resulting value is lower than current, replace it.
  //    Also, add all SSA edges from current assignment.
  // 8. Go to step 4.


// block -> var map -> var map
// stmt -> var map -> var map
