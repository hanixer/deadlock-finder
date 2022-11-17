package deadlockFinder

import translation.SourceToHil
import translation.HilToLil
import translation.LilToSsa
import common.PrettyPrint
import deadlockFinder.cfg.CfgGraph
import org.eclipse.jdt.core.dom.*

import java.nio.file.{Files, Path}

object TranslateSsa:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hil = SourceToHil(node)
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val cfg = CfgGraph(func)
    Files.writeString(Path.of("target/cfgBig.dot"), PrettyPrint.funcToDot(func, cfg))
    println(PrettyPrint(ssa))

end TranslateSsa
