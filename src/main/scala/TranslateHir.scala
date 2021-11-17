package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateHir:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val res = SourceToHir(node)
    println(res.prettyPrint.render(0))

end TranslateHir
