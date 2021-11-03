package deadlockFinder


abstract sealed class AstNode {
  val loc: SourceLoc
}

case class Program(funcs: List[Int], loc: SourceLoc = SourceLoc(1, 1)) extends AstNode

trait Expr extends AstNode

case class IntLiteral(n: Int, loc: SourceLoc) extends Expr

case class Variable(name: String, loc: SourceLoc) extends Expr



