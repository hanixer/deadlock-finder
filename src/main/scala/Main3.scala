package deadlockFinder

import deadlockFinder.analysis.operation.graph.OperationGraphBuilder
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.translation.Util
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main3:
  def main(args: Array[String]): Unit =
    val path = "examples/parallel/MpiSendRecv.java"
    val lil = Util.fileToSsa(path)
    println(PrettyPrint(lil))
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val str = PrettyPrint.operationGraphToDot(operationGraph)
    println(str)
    Files.writeString(Path.of("target/opgraph.dot"), str)
    Files.writeString(Path.of("target/cfg.dot"), PrettyPrint.funcToDot(lil.funcs.head, CfgGraph(lil.funcs.head)))

end Main3
