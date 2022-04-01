package deadlockFinder
package analysis

import hil.{
  AbstractVar,
  BinaryExpr,
  CallExpr,
  Expr,
  UnaryExpr,
}
import lil.*
import common.*

/** Contains various query methods related to uses and defs. */
class UsesAndDefs(uses: List[Use], defs: List[Def]):
  val defUseMap: Map[Def, Set[Use]] =
    defs.map { d =>
      val u = uses.filter(u => u.varInfo == d.varInfo).toSet
      (d, u)
    }.toMap

  val varInfoUseMap: Map[VarInfo, Set[Use]] =
    uses.groupMap(_.varInfo)(identity).map((k, v) => (k, v.toSet))

  val definingExprs: Map[VarInfo, Expr] =
    defs.flatMap { d => d match
      case Def.VDecl(varInfo, VarDecl(_, _, Some(expr), _)) =>
        Some(varInfo, expr)
      case Def.Assign(varInfo, Assignment(_, expr, _)) =>
        Some(varInfo, expr)
      case _ => None
    }.toMap

  def getUsesOfVar(varInfo: VarInfo): Set[Use] = varInfoUseMap.getOrElse(varInfo, Set.empty)

  def getUsesForDef(d: Def): Set[Use] = defUseMap(d)

  def getDefiningExpr(varInfo: VarInfo): Option[Expr] = definingExprs.get(varInfo)

  def getDefiningExpr(v: AbstractVar): Option[Expr] = getDefiningExpr(VarInfo(v))

  def getInitialDefiningExpr(name: String): Option[Expr] = getDefiningExpr(VarInfo(name, Some(0)))

object UsesAndDefs:
  def apply(func: FuncDecl): UsesAndDefs =
    val defsP = funcParamsToDefs(func.params)
    val defsB = func.body.flatMap(getDefsInBlock)
    val uses = func.body.flatMap(getUsesInBlock)

    new UsesAndDefs(uses, defsP ++ defsB)

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
    case vd: VarDecl   => Some(Def(vd))
    case a: Assignment => Some(Def(a))
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

  def getDefs(func: FuncDecl): List[Def] =
    val defsP = funcParamsToDefs(func.params)
    val defsB = func.body.flatMap(getDefsInBlock)
    defsP ++ defsB
