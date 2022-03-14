package deadlockFinder

import deadlockFinder.analysis.operation.graph.OperationGraphBuilder
import deadlockFinder.common.PrettyPrint
import deadlockFinder.translation.Util
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.nio.file.{Files, Path}

object Main3:
  def main(args: Array[String]): Unit =
    val path = "examples/parallel/MpiRecvDeadlock1.java"
    val lil = Util.fileToSsa(path)
    val operationGraph = OperationGraphBuilder(lil.funcs.head)
    val str = PrettyPrint.operationGraphToDot(operationGraph)
    println(str)
    Files.writeString(Path.of("target/opgraph.dot"), str)

end Main3
