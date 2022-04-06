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
    val path = "examples/showcase/ForLoop.java"
    val hil = SourceToHil(JavaParser.parseFile(path))
    val lil = HilToLil(hil)
    val func = lil.funcs.head
    val liveness = new Liveness(func)
    println(PrettyPrint(func))
    assert(liveness.isVariableLiveInBlock("i", "entry") === false)
    assert(liveness.isVariableLiveInBlock("i", "bb1") === false)
    assert(liveness.isVariableLiveInBlock("i", "bb2") === true)
    assert(liveness.isVariableLiveInBlock("i", "end") === false)
    assert(liveness.isVariableLiveInBlock("a", "entry") === true)
    assert(liveness.isVariableLiveInBlock("a", "bb1") === true)
    assert(liveness.isVariableLiveInBlock("a", "bb2") === true)
    assert(liveness.isVariableLiveInBlock("a", "bb3") === true)
    assert(liveness.isVariableLiveInBlock("a", "end") === false)
    assert(liveness.isVariableLiveInBlock("t~1", "bb1") === false)
    assert(liveness.isVariableLiveInBlock("t~1", "bb2") === false)
    assert(liveness.isVariableLiveInBlock("t~1", "bb3") === false)
    assert(liveness.isVariableLiveInBlock("t~1", "end") === false)
    assert(liveness.isVariableLiveInBlock("t~2", "bb1") === false)
    assert(liveness.isVariableLiveInBlock("t~2", "bb2") === false)
    assert(liveness.isVariableLiveInBlock("t~2", "bb3") === false)
    assert(liveness.isVariableLiveInBlock("t~2", "end") === false)
  }
    