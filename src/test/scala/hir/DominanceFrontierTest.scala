package hir

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HirToLil
import deadlockFinder.translation.SourceToHir
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
    val cfg = CfgGraph(nodes, edges, entry)
    val fronts = Dominators.findDominanceFrontiers(cfg)

    assert(fronts("B") === Set("D"))
    assert(fronts("C") === Set("D", "E"))
    assert(fronts("D") === Set("E", "A"))
    assert(fronts("E") === Set())
    assert(fronts("A") === Set("A"))
  }
    