package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir
import translation.HirToLil
import common.PrettyPrint

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateLil:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    println(PrettyPrint(hir))
    println(PrettyPrint(lil))

end TranslateLil
