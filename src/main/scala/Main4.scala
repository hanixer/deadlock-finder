package deadlockFinder

import analysis.opgraph.{InsertAdditionalNodes, OperationGraphBuilder}
import analysis.pnet.PetriNetBuilder
import cfg.CfgGraph
import common.PrettyPrint
import translation.{HilToLil, LoopUnrolling, SourceToHil, Util}

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main4:
  def main(args: Array[String]): Unit =
    val path = "examples/showcase/ForLoop2.java"
    val hil = SourceToHil(JavaParser.parseFile(path))
    val lil = HilToLil(hil)
    val s = PrettyPrint(hil)
    println(s)


