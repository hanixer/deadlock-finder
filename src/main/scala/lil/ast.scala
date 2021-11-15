package deadlockFinder
package lil

import common.*
import hir.{Expr, SimpleExpr, CallExpr}
import org.typelevel.paiges.Doc

case class Program(funcs: List[FuncDecl], loc: SourceLoc = SourceLoc(1, 1))
    extends AstNode:
  def prettyPrint: Doc =
    Doc.fill(Doc.line + Doc.line, funcs.map(_.prettyPrint))

case class FuncDecl(
    name: String,
    params: List[Param],
    retTyp: Type,
    body: List[Block],
    loc: SourceLoc
) extends AstNode:
  def prettyPrint: Doc =
    val ps = params.map(_.prettyPrint)
    val pComma = PrettyPrint.separateComma(ps)
    val b = Doc.fill(Doc.line + Doc.line,  body.map(_.prettyPrint))
    ("func " +: Doc.text(name))
      + ("(" +: pComma :+ "): ")
      + Doc.str(retTyp) + Doc.line + b


trait Stmt extends AstNode

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = (lhs + " = ") +: rhs.prettyPrint

case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc)
    extends Stmt:
  def prettyPrint: Doc =
    val r = rhs match
      case Some(e) => " = " +: e.prettyPrint
      case _ => Doc.empty
    ("var " + name + ": ") +: (Doc.str(t) + r)


case class Block(
    label: String,
    stmts: List[Stmt],
    transfer: Transfer,
    loc: SourceLoc
) extends Stmt:
  def prettyPrint: Doc =
    val body = stmts.map(_.prettyPrint) ++ List(transfer.prettyPrint)
    Doc.text(label + ": ") + Doc.line +
      Doc.fill(Doc.line, body)

case class CallStmt(callExpr: CallExpr) extends Stmt:
  val loc: SourceLoc = callExpr.loc
  def prettyPrint: Doc =
    val args = PrettyPrint.separateComma(callExpr.args.map(_.prettyPrint))
    (callExpr.name + "(") +: (args :+ ")")


trait Transfer extends Stmt

case class Jump(label: String, loc: SourceLoc) extends Transfer:
  def prettyPrint: Doc = Doc.text(s"jump $label")

case class CondJump(
    cond: SimpleExpr,
    thenLabel: String,
    elseLabel: String,
    loc: SourceLoc
) extends Transfer:
  def prettyPrint: Doc = Doc.text(s"condJump $cond $thenLabel $elseLabel")

case class Return(expr: Option[SimpleExpr], loc: SourceLoc) extends Transfer:
  def prettyPrint: Doc =
    "return " +: expr.map(_.prettyPrint).getOrElse(Doc.empty)
