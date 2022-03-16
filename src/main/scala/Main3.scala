package deadlockFinder

import analysis.opgraph.OperationGraphBuilder
import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import translation.Util

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main3:
  def main(args: Array[String]): Unit =
    val path = "examples/parallel/MpiSendRecv3.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val petriNet = PetriNetBuilder(operationGraph)
    Files.writeString(Path.of("target/opgraph.dot"), PrettyPrint.operationGraphToDot(operationGraph))
    Files.writeString(Path.of("target/petrinet.dot"), PrettyPrint.petriNetToDot(petriNet))
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.funcToDot(lil.funcs.head, CfgGraph(lil.funcs.head)))
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.cfgToDot(CfgGraph(lil.funcs.head)))
    Files.writeString(Path.of("target/net.net"), PrettyPrint.petriNetToTina(petriNet))

end Main3
