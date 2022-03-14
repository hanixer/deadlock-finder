package deadlockFinder
package translation

import lil.Program

object Util:
  def fileToSsa(path: String): Program =
    val hil = AssertInsertion(SourceToHil(JavaParser.parseFile(path)))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    ssa

