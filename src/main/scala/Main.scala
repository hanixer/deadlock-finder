package deadlockFinder

import cfg.{CfgGraph, Dominators}
import common.PrettyPrint
import hil.{Expr, IntLiteral, Variable}
import translation.{HilToLil, LilToSsa, SourceToHil}

import deadlockFinder.analysis.ConstantPropagation
import org.eclipse.jdt.core.dom.*

import java.nio.file.{Files, Path}

object Main:
  def main(args: Array[String]): Unit =
    val file = "examples/showcase/Example7.java"
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hil = SourceToHil(node)
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)

    Files.writeString(Path.of("out.ssa"), PrettyPrint(ssa))
    
    val func = ssa.funcs.head
    val consts = ConstantPropagation.computeConstants(func)
    for (varInfo, const) <- consts do
      println(s"$varInfo -> $const")


end Main
