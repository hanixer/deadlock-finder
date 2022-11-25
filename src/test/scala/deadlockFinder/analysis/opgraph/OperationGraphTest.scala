package deadlockFinder
package analysis.opgraph

import analysis.opgraph.IntermediateNode
import analysis.{ConstantPropagation, ProcessRank, VarInfo}
import cfg.CfgGraph
import common.PrettyPrint
import translation.{HilToLil, LilToSsa, SourceToHil, Util}

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class OperationGraphTest extends AnyFunSuite:
  test("construct and compare") {
    val path = "examples/parallel/MpiSendRecv2.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val n1 = new IntermediateNode("entry")
    val n2 = new SendNode(ProcessRank.Concrete(0), 1)
    val n3 = new RecvNode(ProcessRank.Concrete(1), ProcessRank.Concrete(0))
    val n4 = new IntermediateNode("bb2")
    val n5 = new SendNode(ProcessRank.Concrete(1), 0)
    val n6 = new RecvNode(ProcessRank.Concrete(0), ProcessRank.Concrete(1))
    val n7 = new IntermediateNode("end")

    val expected = List(
      (n1, n2),
      (n1, n3),
      (n2, n4),
      (n3, n4),
      (n4, n5),
      (n4, n6),
      (n5, n7),
      (n6, n7)
    )

    // TODO: refresh test
//    checkEdges(expected, operationGraph.edges)
  }

  def checkEdges(expected: List[(Node, Node)], actual: List[(Node, Node)]): Unit =
    assert(expected.length === actual.length,
      s"Expected: ${expected}, \nactual: ${actual}")

    for e1 <- expected do
      if !actual.exists(e2 => compare(e1, e2)) then
        assert(false, s"Actual edges $actual does not contain edge $e1")

  def compare(e1: (Node, Node), e2: (Node, Node)): Boolean =
    compare(e1._1, e2._1) && compare(e1._2, e2._2)

  def compare(n1: Node, n2: Node): Boolean = (n1, n2) match
    case (s1: SendNode, s2: SendNode) =>
      s1.caller == s2.caller && s1.callee == s2.callee
    case (r1: RecvNode, r2: RecvNode) =>
      r1.caller == r2.caller && r1.callee == r2.callee
    case (i1: IntermediateNode, i2: IntermediateNode) =>
      i1.label == i2.label
    case _ => false