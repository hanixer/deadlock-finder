package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.{HirToLil, SourceToHir}

import common.PrettyPrint
import org.eclipse.jdt.core.dom.*

import java.nio.file.Path
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.cfg.Dominators
import java.nio.file.Files

object Main:
  def main(args: Array[String]): Unit =
    // val file = "test/lil/Example3.java"
    // val node: CompilationUnit = JavaParser.parseFile(file)
    // val hir = SourceToHir(node)
    // val lil = HirToLil(hir)
    // println(PrettyPrint(lil))

    val nodes = List("S", "C", "B", "F", "G", "E", "A", "I", "J", "H", "D", "K")
    val edges = List(
      ("S", "C"),
      ("S", "B"),
      ("S", "A"),
      ("C", "F"),
      ("C", "G"),
      ("F", "I"),
      ("I", "K"),
      ("K", "S"),
      ("K", "I"),
      ("G", "I"),
      ("G", "J"),
      ("J", "I"),
      ("B", "E"),
      ("B", "A"),
      ("E", "H"),
      ("H", "E"),
      ("H", "K"),
      ("A", "D")
    )
    val entry = "S"

    val cfg = CfgGraph(nodes, edges, entry)
    val doms = Dominators.findImmediateDominators(cfg)
    val s = "digraph G {\n" +  doms.map((k, v) => s"$k -> $v").mkString("\n") + "\n}"
    Files.writeString(Path.of("out.dot"), s)

end Main
