package deadlockFinder

import cfg.{CfgGraph, Dominators}
import common.PrettyPrint
import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, LilToSsa, SourceToHir}

import org.eclipse.jdt.core.dom.*

import java.nio.file.{Files, Path}
import deadlockFinder.analysis.ConstantPropagation

object Main:
  def main(args: Array[String]): Unit =
    val file = "examples/showcase/Example7.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    val ssa = LilToSsa(lil)

    val func = ssa.funcs.head
    val useDefMap = ConstantPropagation.buildDefUseMap(func)
    for (u, d) <- useDefMap do
      println(s"$u -> $d")

    Files.writeString(Path.of("out.ssa"), PrettyPrint(ssa))

end Main
