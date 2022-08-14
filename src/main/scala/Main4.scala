package deadlockFinder

import analysis.opgraph.{InsertAdditionalNodes, OperationGraphBuilder}
import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import parteval.Reduction
import translation.{LoopUnrolling, SourceToHil, Util}

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main4:
  def main(args: Array[String]): Unit =
    val path = "examples/showcase/ForLoop.java"
    val lil = Util.fileToLil(path)
    val r = new Reduction(lil.funcs.head).transform()
    println(PrettyPrint(r))
