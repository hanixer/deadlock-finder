package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateHir:
  def main(args: Array[String]): Unit =
    val source = java.nio.file.Files.readString(Path.of(args(0)))
    val node: CompilationUnit = parseJava(source)
    val res = SourceToHir(node)
    println(res.prettyPrint.render(0))

  private def parseJava(source: String): CompilationUnit =
    val parser = ASTParser.newParser(AST.JLS16)
    parser.setSource(source.toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array.empty, Array.empty, Array.empty, true)
    parser.setUnitName("Unit name")
    parser.createAST(null).asInstanceOf[CompilationUnit]

end TranslateHir
