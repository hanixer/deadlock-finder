package deadlockFinder
package analysis.opgraph

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HilToLil, LilToSsa, SourceToHil, Util}
import deadlockFinder.JavaParser
import deadlockFinder.analysis.opgraph.IntermediateNode
import deadlockFinder.analysis.{ConstantPropagation, VarInfo}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint

import java.nio.file.Files
import java.nio.file.Path

class OperationGraphTest extends AnyFunSuite:
  test("example 1") {

    val path = "examples/showcase/Example12.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)

    assert(operationGraph.root.isInstanceOf[IntermediateNode])
  }
