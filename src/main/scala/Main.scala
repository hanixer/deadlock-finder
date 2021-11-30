package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, SourceToHir}

import common.PrettyPrint
import org.eclipse.jdt.core.dom.*

import java.nio.file.Path
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.cfg.Dominators
import java.nio.file.Files
import deadlockFinder.translation.LilToSsa

object Main:
  def main(args: Array[String]): Unit =
    val file = "examples/showcase/Example7.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hir = SourceToHir(node)
    val lil = HirToLil(hir)
    Files.writeString(Path.of("out.lil"), PrettyPrint(lil))

    val cfg = CfgGraph(lil.funcs(0))
    val s = PrettyPrint.cfgToDot(cfg)    
    Files.writeString(Path.of("out.dot"), s)

    val btop = LilToSsa.buildBlockToParams(lil.funcs(0), cfg)
    for (k, v) <- btop do
      println(s"$k: ${v.mkString(", ")}")

end Main
