package deadlockFinder
package hir

abstract sealed class AstNode {
  val loc: SourceLoc
}

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

sealed trait Type

case object IntType extends Type:
  override def toString: String = "int"

case object DoubleType extends Type:
  override def toString: String = "double"

case object FloatType extends Type:
  override def toString: String = "float"

case object BooleanType extends Type:
  override def toString: String = "boolean"

case object VoidType extends Type:
  override def toString: String = "void"

case class ClassType() extends Type:
  override def toString: String = "someClass"

enum BinaryOp:
  case Plus, Minus, Times, Divide, Less, Greater, LessEquals, GreaterEquals,
  Equals, And, Or

  override def toString(): String = this match
    case Plus          => "+"
    case Minus         => "-"
    case Times         => "*"
    case Divide        => "/"
    case Less          => "<"
    case Greater       => ">"
    case LessEquals    => "<="
    case GreaterEquals => ">="
    case Equals        => "=="
    case And           => "&&"
    case Or            => "||"

enum UnaryOp:
  case Not

  override def toString(): String = this match
    case Not => "!"
