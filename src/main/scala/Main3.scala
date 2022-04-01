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
    val path = "examples/parallel/Loop1.java"
    val lil = Util.fileToSsa(path)
    val func = lil.funcs.head
    val cfg = CfgGraph(func)
    print(PrettyPrint(lil))
    Files.writeString(Path.of("target/cfgBig.dot"), PrettyPrint.funcToDot(func, cfg))
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.cfgToDot(cfg))
    val operationGraph = OperationGraphBuilder(func)
    Files.writeString(Path.of("target/opgraph.dot"), PrettyPrint.operationGraphToDot(operationGraph))
    val petriNet = PetriNetBuilder(operationGraph)
    Files.writeString(Path.of("target/petrinet.dot"), PrettyPrint.petriNetToDot(petriNet))
    Files.writeString(Path.of("target/net.net"), PrettyPrint.petriNetToTina(petriNet))

end Main3
