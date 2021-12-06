package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir
import translation.HirToLil
import translation.LilToSsa
import common.PrettyPrint

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateSsa:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    val ssa = LilToSsa(lil)
    println(PrettyPrint(ssa))

end TranslateSsa
