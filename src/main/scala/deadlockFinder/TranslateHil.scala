package deadlockFinder
import translation.{AssertInsertion, SourceToHil}
import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object TranslateHil:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val node: CompilationUnit = JavaParser.parseFile(file)
    val res = AssertInsertion(SourceToHil(node))
    println(res.prettyPrint.render(0))

end TranslateHil
