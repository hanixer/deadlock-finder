package deadlockFinder

import analysis.opgraph.{InsertAdditionalNodes, OperationGraphBuilder}
import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import translation.Util

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main3:
  def main(args: Array[String]): Unit =
    val path = "examples/parallel/MpiSendRecvConditions4.java"
    val lil = Util.fileToSsa(path)
    Files.writeString(Path.of("target/cfgBig.dot"), PrettyPrint.funcToDot(lil.funcs.head, CfgGraph(lil.funcs.head)))
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.cfgToDot(CfgGraph(lil.funcs.head)))
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    Files.writeString(Path.of("target/opgraph.dot"), PrettyPrint.operationGraphToDot(operationGraph))
    val petriNet = PetriNetBuilder(operationGraph)
    Files.writeString(Path.of("target/petrinet.dot"), PrettyPrint.petriNetToDot(petriNet))
    Files.writeString(Path.of("target/net.net"), PrettyPrint.petriNetToTina(petriNet))

end Main3
