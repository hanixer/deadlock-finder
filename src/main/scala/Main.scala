package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path
import deadlockFinder.hir.PrettyPrint

object Main:
  def main(args: Array[String]): Unit =
    val source = java.nio.file.Files.readString(Path.of("examples/showcase/Example2.java"))
    val node: CompilationUnit = parseJava(source)
    val res = SourceToHir(node)
    println(PrettyPrint(res))    

  private def parseJava(source: String): CompilationUnit =
    val parser = ASTParser.newParser(AST.JLS16)
    parser.setSource(source.toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array.empty, Array.empty, Array.empty, true)
    parser.setUnitName("Unit name")
    parser.createAST(null).asInstanceOf[CompilationUnit]

end Main

