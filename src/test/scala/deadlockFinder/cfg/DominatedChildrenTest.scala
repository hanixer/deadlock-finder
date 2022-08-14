package deadlockFinder
package cfg

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HilToLil
import deadlockFinder.translation.SourceToHil
import deadlockFinder.JavaParser
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint

class DominatedChildrenTest extends AnyFunSuite:
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
      ("C", "E")
    )
    val entry = "r"
    val exit = "D"
    val cfg = CfgGraph(nodes, edges, entry, exit)
    val doms = Dominators(cfg)

    assert(doms.getDomTreeChildren("A") === Set("B", "C", "D", "E"))
    assert(doms.getDomTreeChildren("B") === Set())
    assert(doms.getDomTreeChildren("C") === Set())
    assert(doms.getDomTreeChildren("D") === Set())
    assert(doms.getDomTreeChildren("E") === Set())
  }
