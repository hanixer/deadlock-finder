package deadlockFinder

import analysis.PredicatesPropagation
import analysis.opgraph.{InsertAdditionalNodes, OperationGraphBuilder}
import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import parteval.Reduction
import translation.*

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main:
  def main(args: Array[String]): Unit =
    val path = args(0)
    val hil = AssertInsertion(SourceToHil(JavaParser.parseFile(path)))
    val lil = HilToLil(hil)
    val func = LilToSsa(new Reduction(lil.funcs.head).transform())
    val cfg = CfgGraph(func)
    Files.writeString(Path.of("target/cfgBig.dot"), PrettyPrint.funcToDot(func, cfg))
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.cfgToDot(cfg))
    val operationGraph = OperationGraphBuilder(func)
    Files.writeString(Path.of("target/opgraph.dot"), PrettyPrint.operationGraphToDot(operationGraph))
    val petriNet = PetriNetBuilder(operationGraph)
    Files.writeString(Path.of("target/petrinet.dot"), PrettyPrint.petriNetToDot(petriNet))
    Files.writeString(Path.of("target/net.net"), PrettyPrint.petriNetToTina(petriNet))

end Main
