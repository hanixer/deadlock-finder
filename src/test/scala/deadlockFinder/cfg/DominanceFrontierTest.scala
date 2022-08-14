package deadlockFinder
package cfg

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HilToLil
import deadlockFinder.translation.SourceToHil
import deadlockFinder.JavaParser
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.cfg.Dominators

class DominanceFrontierTest extends AnyFunSuite:
  test("example 1") {
    val nodes = List("r", "A", "B", "C", "D", "E")
    val edges = List(
      ("r", "A"),
      ("A", "B"),
      ("A", "C"),
      ("B", "D"),
      ("D", "A"),
      ("D", "E"),
      ("C", "D"),
      ("C", "E"),
    )
    val entry = "r"
    val exit = "D"
    val cfg = CfgGraph(nodes, edges, entry, exit)
    val doms = Dominators(cfg)

    assert(doms.getDominanceFrontier("B") === Set("D"))
    assert(doms.getDominanceFrontier("C") === Set("D", "E"))
    assert(doms.getDominanceFrontier("D") === Set("E", "A"))
    assert(doms.getDominanceFrontier("E") === Set())
    assert(doms.getDominanceFrontier("A") === Set("A"))
  }
    