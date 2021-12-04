package deadlockFinder

import cfg.{CfgGraph, Dominators}
import common.PrettyPrint
import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, LilToSsa, SourceToHir}

import org.eclipse.jdt.core.dom.*

import java.nio.file.{Files, Path}

object Main:
  def main(args: Array[String]): Unit =
    val file = "examples/showcase/Example7.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    val ssa = LilToSsa(lil)

    Files.writeString(Path.of("out.ssa"), PrettyPrint(ssa))

end Main
