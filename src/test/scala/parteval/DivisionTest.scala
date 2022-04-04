package deadlockFinder
package parteval

import analysis.{UsesAndDefs, VarInfo}
import cfg.CfgGraph
import common.PrettyPrint
import hil.IntLiteral
import parteval.Division
import translation.{HilToLil, LilToSsa, SourceToHil, Util}

import org.scalatest.funsuite.AnyFunSuite

class DivisionTest extends AnyFunSuite:
  test("example 1") {
    val path = "examples/showcase/ForLoop.java"
    val lil = Util.fileToLil(path)
    val func = lil.funcs.head
    val division = new Division(func)
    assert(division.isDynamic("i") === false)
    assert(division.isDynamic("a") === true)
  }
  test("example 2") {
    val path = "examples/showcase/ForLoop2.java"
    val lil = Util.fileToLil(path)
    val func = lil.funcs.head
    val division = new Division(func)
    assert(division.isDynamic("i") === false)
    assert(division.isDynamic("j") === false)
    assert(division.isDynamic("a") === false)
  }
    