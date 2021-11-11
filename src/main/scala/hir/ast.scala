package deadlockFinder
package hir

import common.*

case class Program(funcs: List[FuncDecl], loc: SourceLoc = SourceLoc(1, 1))
    extends AstNode

    
case class FuncDecl(
    name: String,
    params: List[Param],
    retTyp: Type,
    body: Block,
    loc: SourceLoc
) extends AstNode

case class Param(name: String, typ: Type, loc: SourceLoc) extends AstNode


trait Stmt extends AstNode

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt

case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc)
    extends Stmt

case class IfThenElse(
    cond: SimpleExpr,
    thenStmt: Stmt,
    elseStmt: Option[Stmt],
    loc: SourceLoc
) extends Stmt

case class While(cond: SimpleExpr, body: Stmt, loc: SourceLoc) extends Stmt

case class Loop(body: Stmt, loc: SourceLoc) extends Stmt

case class Block(stmts: List[Stmt], loc: SourceLoc) extends Stmt

case class Break(loc: SourceLoc) extends Stmt

case class Continue(loc: SourceLoc) extends Stmt

case class Return(expr: Option[Expr], loc: SourceLoc) extends Stmt

case class CallStmt(callExpr: CallExpr) extends Stmt {
  val loc: SourceLoc = callExpr.loc
}


trait Expr extends AstNode

trait SimpleExpr extends Expr

case class IntLiteral(n: Int, loc: SourceLoc) extends SimpleExpr

case class Variable(name: String, loc: SourceLoc) extends SimpleExpr

case class BinaryExpr(
    op: BinaryOp,
    lhs: SimpleExpr,
    rhs: SimpleExpr,
    loc: SourceLoc
) extends Expr

case class UnaryExpr(op: UnaryOp, e: SimpleExpr, loc: SourceLoc) extends Expr

case class CallExpr(name: String, args: List[SimpleExpr], loc: SourceLoc)
    extends Expr

case class UnsupportedConstruct(loc: SourceLoc) extends SimpleExpr, Stmt
