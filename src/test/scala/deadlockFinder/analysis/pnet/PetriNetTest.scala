package deadlockFinder
package analysis.pnet

import analysis.opgraph.{IntermediateNode, OperationGraphBuilder}
import analysis.{ConstantPropagation, ProcessRank, VarInfo}
import cfg.CfgGraph
import common.PrettyPrint
import translation.{HilToLil, LilToSsa, SourceToHil, Util}

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class PetriNetTest extends AnyFunSuite
// TODO: refresh tests
/*
  test("basic structure") {
    val path = "examples/parallel/MpiSendRecv.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val petriNet = PetriNetBuilder(operationGraph)

    assert(petriNet.successors(petriNet.root).length === 1)

    val n1 = petriNet.successors(petriNet.root).head
    assert(n1.isInstanceOf[Transition])
    assert(petriNet.successors(n1).length === 4)
    assert(petriNet.successors(n1).forall { n => n.isInstanceOf[Place] })

    // Transition with two successors and two predecessors
    assert(petriNet.nodes.exists { n =>
      n.isInstanceOf[Transition] && petriNet.successors(n).length == 2 && petriNet.predecessors(n).length == 2
    })

    // Transition with four predecessors
    assert(petriNet.nodes.exists { n =>
      n.isInstanceOf[Transition] && petriNet.successors(n).length == 1 && petriNet.predecessors(n).length == 4
    })

    // Transition with four predecessors
    assert(petriNet.nodes.exists { n =>
      n.isInstanceOf[Place] && petriNet.successors(n).isEmpty && petriNet.predecessors(n).length == 1
    })
  }

*/