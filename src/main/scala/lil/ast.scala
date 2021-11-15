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
  def prettyPrint: Doc = Doc.empty


trait Stmt extends AstNode

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt:
  def prettyPrint: Doc = Doc.empty


case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc)
    extends Stmt:
  def prettyPrint: Doc = Doc.empty


case class Block(
    label: String,
    stmts: List[Stmt],
    transfer: Transfer,
    loc: SourceLoc
) extends Stmt:
  def prettyPrint: Doc = Doc.empty


case class CallStmt(callExpr: CallExpr) extends Stmt:
  val loc: SourceLoc = callExpr.loc
  def prettyPrint: Doc = Doc.empty


trait Transfer extends Stmt

case class Jump(label: String, loc: SourceLoc) extends Transfer:
  def prettyPrint: Doc = Doc.empty


case class CondJump(
    cond: SimpleExpr,
    thenLabel: String,
    elseLabel: String,
    loc: SourceLoc
) extends Transfer:
  def prettyPrint: Doc = Doc.empty


case class Return(expr: Option[SimpleExpr], loc: SourceLoc) extends Transfer:
  def prettyPrint: Doc = Doc.empty
