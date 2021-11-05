package deadlockFinder
package hir


abstract sealed class AstNode {
  val loc: SourceLoc
}

case class Program(funcs: List[FuncDecl], loc: SourceLoc = SourceLoc(1, 1)) extends AstNode

case class FuncDecl(name: String, params: List[Param], retTyp: Type, body: Block, loc: SourceLoc) extends AstNode

case class Param(name: String, typ: Type, loc: SourceLoc) extends AstNode


trait Stmt extends AstNode

case class Assignment(lhs: String, rhs: Expr, loc: SourceLoc) extends Stmt

case class VarDecl(name: String, t: Type, rhs: Option[Expr], loc: SourceLoc) extends Stmt

case class IfThenElse(cond: Expr, tBranch: Stmt, eBranch: Option[Stmt], loc: SourceLoc) extends Stmt

case class While(cond: Expr, body: Stmt, loc: SourceLoc) extends Stmt

case class Block(stmts: List[Stmt], loc: SourceLoc) extends Stmt

case class Break(loc: SourceLoc) extends Stmt

case class Continue(loc: SourceLoc) extends Stmt

case class Return(expr: Option[Expr], loc: SourceLoc) extends Stmt

case class CallStmt(callExpr: CallExpr) extends Stmt {
  val loc: SourceLoc = callExpr.loc
}


trait Expr extends AstNode

case class IntLiteral(n: Int, loc: SourceLoc) extends Expr

case class Variable(name: String, loc: SourceLoc) extends Expr

case class BinaryOpExpr(op: BinaryOp, lhs: Expr, rhs: Expr, loc: SourceLoc) extends Expr

case class CallExpr(name: String, args: List[Expr], loc: SourceLoc) extends Expr


sealed trait Type

case class IntType() extends Type

case class DoubleType() extends Type

case class VoidType() extends Type

case class ClassType() extends Type


enum BinaryOp {
  case Plus, Minus, Times, Divide, Less, Greater, LessEquals, GreaterEquals, Equals, And, Or

  override def toString(): String = this match
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case Divide => "/"
    case Less => "<"
    case Greater => ">"
    case LessEquals => "<="
    case GreaterEquals => ">="
    case Equals => "=="
    case And => "&&"
    case Or => "||"
}
