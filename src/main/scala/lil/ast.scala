package deadlockFinder
package lil

import common.*
import hir.{Expr, SimpleExpr, CallExpr}

case class Program(funcs: List[FuncDecl], loc: SourceLoc = SourceLoc(1, 1))
    extends AstNode

case class FuncDecl(
    name: String,
    params: List[Param],
    retTyp: Type,
    body: List[Block],
    loc: SourceLoc
) extends AstNode


trait Stmt extends AstNode

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt

case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc)
    extends Stmt

case class Block(
    label: String,
    stmts: List[Stmt],
    transfer: Transfer,
    loc: SourceLoc
) extends Stmt

case class CallStmt(callExpr: CallExpr) extends Stmt {
  val loc: SourceLoc = callExpr.loc
}

trait Transfer extends Stmt

case class Jump(label: String, loc: SourceLoc) extends Transfer

case class CondJump(
    cond: SimpleExpr,
    thenLabel: String,
    elseLabel: String,
    loc: SourceLoc
) extends Transfer

case class Return(expr: Option[SimpleExpr], loc: SourceLoc) extends Transfer


// trait Expr extends AstNode

// trait SimpleExpr extends Expr

// case class IntLiteral(n: Int, loc: SourceLoc) extends SimpleExpr

// case class Variable(name: String, loc: SourceLoc) extends SimpleExpr

// case class BinaryExpr(
//     op: BinaryOp,
//     lhs: SimpleExpr,
//     rhs: SimpleExpr,
//     loc: SourceLoc
// ) extends Expr

// case class UnaryExpr(op: UnaryOp, e: SimpleExpr, loc: SourceLoc) extends Expr

// case class CallExpr(name: String, args: List[SimpleExpr], loc: SourceLoc)
//     extends Expr

// case class UnsupportedConstruct(loc: SourceLoc) extends SimpleExpr, Stmt
