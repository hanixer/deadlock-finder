package deadlockFinder

import translation.SourceToHil
import translation.HilToLil
import translation.LilToSsa
import common.PrettyPrint

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateSsa:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hil = SourceToHil(node)
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    println(PrettyPrint(ssa))

end TranslateSsa
