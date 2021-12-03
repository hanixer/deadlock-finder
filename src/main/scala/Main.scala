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

    val func = lil.funcs.head
    val cfg = CfgGraph(func)
    val s = PrettyPrint.funcToDot(func, cfg)
    Files.writeString(Path.of("out.dot"), s)

    val btop = LilToSsa.buildBlockToParams(func, cfg)
    val func2 = LilToSsa.addBlockParams(func, btop)
    Files.writeString(Path.of("out.lil"), PrettyPrint(func2))

end Main
