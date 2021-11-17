package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, SourceToHir}

import common.PrettyPrint
import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object Main:
  def main(args: Array[String]): Unit =
    val file = "test/lil/Example2.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    println(PrettyPrint(lil))

end Main

