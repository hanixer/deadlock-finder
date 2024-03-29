package deadlockFinder
package hil

import common.*
import expr.*

import org.typelevel.paiges.Doc

case class Program(funcs: List[FuncDecl], tempCounter: Int, loc: SourceLoc = SourceLoc(1, 1)) extends AstNode:
  def prettyPrint: Doc =
    Doc.fill(Doc.line + Doc.line, funcs.map(_.prettyPrint))

case class FuncDecl(
    name: String,
    params: List[Param],
    retTyp: Type,
    body: Block,
    loc: SourceLoc
) extends AstNode:
  def prettyPrint: Doc =
    val ps = params.map(_.prettyPrint)
    val pComma = PrettyPrint.separateComma(ps)
    val b = body.prettyPrint
    ("func " +: Doc.text(name))
      + ("(" +: pComma :+ "): ")
      + Doc.str(retTyp) + Doc.space + b

trait Stmt extends AstNode

case class Assert(expr: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = "assert " +: expr.prettyPrint

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = (lhs + " = ") +: rhs.prettyPrint

case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc =
    val r = rhs match
      case Some(e) => " = " +: e.prettyPrint
      case _       => Doc.empty
    ("var " + name + ": ") +: (Doc.str(t) + r)

case class IfThenElse(
    cond: SimpleExpr,
    thenBlock: Block,
    elseBlock: Option[Block],
    loc: SourceLoc
) extends Stmt:
  def prettyPrint: Doc =
    val elseBranch = elseBlock match
      case Some(s) => " else " +: s.prettyPrint
      case _       => Doc.empty
    val ifCond = ("if (" +: cond.prettyPrint) :+ ") "
    ifCond + thenBlock.prettyPrint + elseBranch

case class WhileLoop(condBlock: Block, condition: SimpleExpr, body: Block, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = Doc.text("while ") +
    (if condBlock.stmts.isEmpty then Doc.empty else condBlock.prettyPrint + Doc.space) +
    PrettyPrint.inParens(condition.prettyPrint) + Doc.space +
    body.prettyPrint

case class Block(stmts: List[Stmt], loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc =
    val b = Doc.fill(Doc.line, stmts.map(_.prettyPrint))
    (Doc.text("{") + Doc.line + b).nested(2) + (Doc.line :+ "}")

case class Break(loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = Doc.text("break")

case class Continue(loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = Doc.text("continue")

case class Return(expr: Option[Expr], loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc =
    "return " +: expr.map(_.prettyPrint).getOrElse(Doc.empty)

case class CallStmt(callExpr: CallExpr) extends Stmt:
  val loc: SourceLoc = callExpr.loc
  def prettyPrint: Doc =
    val args = PrettyPrint.separateComma(callExpr.args.map(_.prettyPrint))
    (callExpr.name + "(") +: (args :+ ")")

case class UnsupportedConstruct(loc: SourceLoc) extends SimpleExpr, Stmt:
  def prettyPrint: Doc = Doc.text(s"[?stmt/expr:(${loc.line}, ${loc.column})]")
