package deadlockFinder

import translation.SourceToHil
import translation.HilToLil
import common.PrettyPrint
import deadlockFinder.cfg.CfgGraph
import org.eclipse.jdt.core.dom.*

import java.nio.file.{Files, Path}

object TranslateLil:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val hil = SourceToHil(node)
    val lil = HilToLil(hil)
    val func = lil.funcs(0)
    val cfg = CfgGraph(func)
    Files.writeString(Path.of("target/cfgBig.dot"), PrettyPrint.funcToDot(func, cfg))
    println(PrettyPrint(lil))

end TranslateLil
