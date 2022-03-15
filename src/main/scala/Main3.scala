package deadlockFinder

import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import translation.Util

import deadlockFinder.analysis.opgraph.OperationGraphBuilder
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main3:
  def main(args: Array[String]): Unit =
    val path = "examples/parallel/MpiSendRecv2.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val petriNet = PetriNetBuilder(operationGraph)
    Files.writeString(Path.of("target/opgraph.dot"), PrettyPrint.operationGraphToDot(operationGraph))
    Files.writeString(Path.of("target/petrinet.dot"), PrettyPrint.petriNetToDot(petriNet))

end Main3
