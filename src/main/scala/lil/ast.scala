package deadlockFinder
package lil

import common.*
import expr.*
import org.typelevel.paiges.Doc

case class Program(funcs: List[FuncDecl], loc: SourceLoc = SourceLoc(1, 1))
    extends AstNode:
  def prettyPrint: Doc =
    Doc.fill(Doc.line + Doc.line, funcs.map(_.prettyPrint))
  def funcByName(name: String): Option[FuncDecl] =
    funcs.find(_.name == name)

case class FuncDecl(
    name: String,
    params: List[Param],
    retTyp: Type,
    declarations: List[VarDecl],
    body: List[Block],
    entryLabel: String,
    exitLabel: String,
    loc: SourceLoc
) extends AstNode:
  lazy val labelToBlock: Map[String, Block] =
    body.map(b => (b.label, b)).toMap
  def prettyPrint: Doc =
    val ps = params.map(_.prettyPrint)
    val b = Doc.fill(Doc.line + Doc.line, body.map(_.prettyPrint))
    ("func " +: Doc.text(name))
      + (PrettyPrint.inParensAndComma(ps) :+ ": ")
      + Doc.str(retTyp) + Doc.line + b

case class BlockParam(v: AbstractVar, typ: Type) extends AstNode:
  val loc: SourceLoc = v.loc
  def prettyPrint: Doc =
    v.prettyPrint + Doc.text(": ") + Doc.str(typ)

object BlockParam:
  def apply(param: Param): BlockParam =
    BlockParam(Variable(param.name, param.loc), param.typ)

case class Block(
    label: String,
    params: List[BlockParam],
    stmts: List[Stmt],
    transfer: Transfer,
    loc: SourceLoc
) extends AstNode:
  def prettyPrint: Doc =
    val body = stmts.map(_.prettyPrint) ++ List(transfer.prettyPrint)
    val ps =
      if params.isEmpty then Doc.empty
      else PrettyPrint.inParensAndComma(params.map(_.prettyPrint))
    Doc.text(label) + ps + Doc.text(":") + Doc.line +
      Doc.fill(Doc.line, body)

object Block:
  def apply(
      label: String,
      stmts: List[Stmt],
      transfer: Transfer,
      loc: SourceLoc
  ): Block = Block(label, List.empty, stmts, transfer, loc)

trait Stmt extends AstNode

case class Assert(expr: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = "assert " +: expr.prettyPrint

case class Assignment(lhs: AbstractVar, rhs: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = 
    (lhs.prettyPrint :+ " = ") + rhs.prettyPrint

case class VarDecl(v: AbstractVar, typ: Type, loc: SourceLoc)
    extends Stmt:
  def prettyPrint: Doc =
    Doc.text("var ") + v.prettyPrint + Doc.text(": ") + Doc.str(typ)

case class CallStmt(callExpr: CallExpr) extends Stmt:
  val loc: SourceLoc = callExpr.loc
  def prettyPrint: Doc =
    callExpr.prettyPrint

trait Transfer extends Stmt

case class Jump(label: String, vars: List[AbstractVar], loc: SourceLoc)
    extends Transfer:
  def prettyPrint: Doc =
    Doc.text(s"jump $label") + PrettyPrint.argsOrEmpty(vars)

object Jump:
  def apply(label: String, loc: SourceLoc): Jump = Jump(label, List.empty, loc)

case class CondJump(
    cond: SimpleExpr,
    thenLabel: String,
    thenArgs: List[AbstractVar],
    elseLabel: String,
    elseArgs: List[AbstractVar],
    loc: SourceLoc
) extends Transfer:
  def prettyPrint: Doc =
    val c = cond.prettyPrint        
    Doc.text("condJump ") + c + Doc.space +
      Doc.text(thenLabel) + PrettyPrint.argsOrEmpty(thenArgs) + Doc.space +
      Doc.text(elseLabel) + PrettyPrint.argsOrEmpty(elseArgs)

object CondJump:
  def apply(
      cond: SimpleExpr,
      thenLabel: String,
      elseLabel: String,
      loc: SourceLoc
  ): CondJump = CondJump(cond, thenLabel, List.empty, elseLabel, List.empty, loc)

case class Return(expr: Option[SimpleExpr], loc: SourceLoc) extends Transfer:
  def prettyPrint: Doc =
    "return" +: expr.map(Doc.space + _.prettyPrint).getOrElse(Doc.empty)

case class SsaVariable(name: String, index: Int, loc: SourceLoc) extends AbstractVar:
  def prettyPrint: Doc =
    Doc.text(s"$name.$index")
