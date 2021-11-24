package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, SourceToHir}

import common.PrettyPrint
import org.eclipse.jdt.core.dom.*

import java.nio.file.Path
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.cfg.Dominators

object Main:
  def main(args: Array[String]): Unit =
    val file = "test/lil/Example3.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    val cfg = CfgGraph(lil.funcs(0))
    println(PrettyPrint(lil))
    println(Dominators.rpostOrder(cfg))

end Main

