package deadlockFinder

import org.eclipse.jdt.core.dom.{AST, ASTParser, ASTVisitor, AbstractTypeDeclaration, CompilationUnit, FieldAccess, TypeDeclaration}
import deadlockFinder.hir.{Expr, IntLiteral, Variable}

object Main {
  def decide(k: Expr): Unit = k match {
    case IntLiteral(n, _) => println(s"${n} is the literal")
    case Variable(name, _) => println(s"${name} is the variable!")
    case _ => println("don't rellay know...")
  }

  def main(args: Array[String]): Unit = {
    playWithEclipse

    val loc = SourceLoc(32, 4)
    decide(IntLiteral(3, loc))
    decide(Variable("3", loc))
    decide(null)
  }

  private def playWithEclipse = {
    val parser = ASTParser.newParser(AST.JLS16)
    parser.setSource("class Foo {} class booka".toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array.empty, Array.empty, Array.empty, false)
    parser.setUnitName("Grooo")
    val node = parser.createAST(null).asInstanceOf[CompilationUnit]
    node.accept(Visitor)
  }

}

object Visitor extends ASTVisitor {
  override def visit(node: CompilationUnit): Boolean = {
    println("Compilation unit is here!")
    true
  }
  override def visit(node: TypeDeclaration): Boolean = {
    println("Here is a type: " + node.getName.getIdentifier)
    false
  }
}

