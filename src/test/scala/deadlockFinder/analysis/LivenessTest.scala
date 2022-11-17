package deadlockFinder
package analysis

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HilToLil, LilToSsa, SourceToHil}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser
import deadlockFinder.analysis.{UsesAndDefs, VarInfo}
import deadlockFinder.expr.*

class LivenessTest extends AnyFunSuite:
  test("example 1") {
    val path = "examples/showcase/Example6_1.java"
    val hil = SourceToHil(JavaParser.parseFile(path))
    val lil = HilToLil(hil)
    val func = lil.funcs.head
    val liveness = new Liveness(func)
    println(PrettyPrint(func))
    assert(!liveness.isVariableLiveInBlock("x", "end"))
    assert(!liveness.isVariableLiveInBlock("y", "end"))
    assert(!liveness.isVariableLiveInBlock("z", "end"))
    assert(liveness.isVariableLiveInBlock("x", "bb4"))
    assert(liveness.isVariableLiveInBlock("y", "bb4"))
    assert(!liveness.isVariableLiveInBlock("z", "bb4"))
    assert(liveness.isVariableLiveInBlock("x", "bb2"))
    assert(liveness.isVariableLiveInBlock("y", "bb2"))
    assert(!liveness.isVariableLiveInBlock("z", "bb2"))
    assert(!liveness.isVariableLiveInBlock("x", "entry"))
    assert(!liveness.isVariableLiveInBlock("y", "entry"))
    assert(!liveness.isVariableLiveInBlock("z", "entry"))
  }
    