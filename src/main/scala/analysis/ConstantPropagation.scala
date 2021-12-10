package deadlockFinder
package analysis

import hir.{
  AbstractVar,
  BinaryExpr,
  CallExpr,
  Expr,
  IntLiteral,
  SimpleExpr,
  UnaryExpr,
  Variable
}
import lil.*
import common.*

object ConstantPropagation:
  /** Constant abstract value. */
  enum ConstantAbsVal:
    case Undefined
    case Constant(n: Int)
    case NotConstant

  def isLower(v1: ConstantAbsVal, v2: ConstantAbsVal): Boolean = v1 match
    case ConstantAbsVal.NotConstant => true
    case ConstantAbsVal.Constant(_) =>
      v2 match
        case ConstantAbsVal.Undefined => true
        case _                        => false
    case ConstantAbsVal.Undefined => false

  type ConstantsMap = Map[VarInfo, ConstantAbsVal]

  def evalBinop(op: BinaryOp, n1: Int, n2: Int): ConstantAbsVal = op match
    case BinaryOp.Plus   => ConstantAbsVal.Constant(n1 + n2)
    case BinaryOp.Minus  => ConstantAbsVal.Constant(n1 - n2)
    case BinaryOp.Times  => ConstantAbsVal.Constant(n1 * n2)
    case BinaryOp.Divide => ConstantAbsVal.Constant(n1 / n2)
    case _               => ConstantAbsVal.NotConstant

  /** Collect immediate constants and add to map. Other variables marked
    * undefined.
    */
  def getImmediateConstants(func: FuncDecl): ConstantsMap =
    def visitSimpleExpr(expr: SimpleExpr): ConstantAbsVal = expr match
      case i: IntLiteral => ConstantAbsVal.Constant(i.n)
      case _             => ConstantAbsVal.Undefined

    def visitExpr(expr: Expr): ConstantAbsVal = expr match
      case BinaryExpr(op, IntLiteral(n1, _), IntLiteral(n2, _), _) =>
        evalBinop(op, n1, n2)
      case se: SimpleExpr => visitSimpleExpr(se)
      case _              => ConstantAbsVal.Undefined

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
      val paramsM =
        block.params.flatMap(p => Some(VarInfo(p.v), ConstantAbsVal.Undefined))
      val stmtsM = block.stmts.flatMap(visitStmt)
      paramsM ++ stmtsM

    func.body.flatMap(visitBlock).toMap

  /** Collect map from use to def */
  def buildUseDefMap(func: FuncDecl): Map[Use, Def] =
    def getUsesInExpr(expr: Expr): List[VarInfo] = expr match
      case b: BinaryExpr  => getUsesInExpr(b.lhs) ++ getUsesInExpr(b.rhs)
      case u: UnaryExpr   => getUsesInExpr(u.e)
      case c: CallExpr    => c.args.flatMap(getUsesInExpr)
      case v: AbstractVar => List(VarInfo(v))
      case _              => List.empty

    def getUsesInStmt(stmt: Stmt): List[Use] = stmt match
      case vd: VarDecl =>
        val varInfos = vd.rhs.map(getUsesInExpr).getOrElse(List.empty)
        varInfos.map(vi => Use.VDecl(vi, vd))
      case a: Assignment =>
        val varInfos = getUsesInExpr(a.rhs)
        varInfos.map(vi => Use.Assign(vi, a))
      case _ => List.empty

    def blockArgsToUses(args: List[AbstractVar], label: String): List[Use] =
      args
        .map(VarInfo.apply)
        .zipWithIndex
        .map((vi, i) => Use.BParam(vi, label, i))

    def getUsesInTransfer(transfer: Transfer): List[Use] = transfer match
      case j: Jump => blockArgsToUses(j.vars, j.label)
      case cj: CondJump =>
        blockArgsToUses(cj.thenArgs, cj.thenLabel) ++ blockArgsToUses(
          cj.elseArgs,
          cj.elseLabel
        )
      case _ => List.empty

    def getDefInStmt(stmt: Stmt): Option[Def] = stmt match
      case vd: VarDecl   => Some(Def.VDecl(VarInfo(vd.v), vd))
      case a: Assignment => Some(Def.Assign(VarInfo(a.lhs), a))
      case _             => None

    def blockParamsToDefs(params: List[BlockParam], label: String): List[Def] =
      params.zipWithIndex.map((p, i) => Def.BParam(VarInfo(p.v), label, i))

    def funcParamsToDefs(params: List[Param]): List[Def] =
      params.zipWithIndex.map((p, i) => Def.FParam(VarInfo(p.name), i))

    def getUsesInBlock(block: Block): List[Use] =
      block.stmts.flatMap(getUsesInStmt) ++ getUsesInTransfer(block.transfer)

    def getDefsInBlock(block: Block): List[Def] =
      blockParamsToDefs(block.params, block.label) ++ block.stmts.flatMap(
        getDefInStmt
      )

    def mkUseDefMap(uses: List[Use], defs: List[Def]): Map[Use, Def] =
      val pairs = uses.map(u =>
          val d: Def = 
            defs.find(d => d.varInfo == u.varInfo)
                .getOrElse(Def.Undefined(u.varInfo))
          (u, d))
      pairs.toMap

    val defsP = funcParamsToDefs(func.params)
    val defsB = func.body.flatMap(getDefsInBlock)
    val uses = func.body.flatMap(getUsesInBlock)

    mkUseDefMap(uses, defsP ++ defsB)

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
