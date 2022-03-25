package deadlockFinder
package analysis.opgraph

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Inserts additional intermediate nodes to operation graph,
  * so that splits and merges inside one process be more explicit. */
object InsertAdditionalNodes:
  def apply(operationGraph: OperationGraph): OperationGraph =
    val toDelete = mutable.HashSet[Edge]()
    val toAdd = ListBuffer[Edge]()

    def handleNeighbours(nodes: List[Node], curr: Node, isSuccessors: Boolean): Unit =
      val nodesByProcess = nodes.flatMap(makeProcessNodePair).groupMap(_._1)(_._2)

      for (_, nodes) <- nodesByProcess if nodes.lengthCompare(1) > 0 do
        val intermediateNode = new IntermediateNode("")
        if isSuccessors then
          toDelete ++= nodes.map { n => (curr, n) }
          toAdd ++= nodes.map { n => (intermediateNode, n) }
          toAdd += ((curr, intermediateNode))
        else
          val d = nodes.map { n => (n, curr) }
          val a = nodes.map { n => (n, intermediateNode) }
          toDelete ++= nodes.map { n => (n, curr) }
          toAdd ++= nodes.map { n => (n, intermediateNode) }
          toAdd += ((intermediateNode, curr))

    for curr <- operationGraph.nodes do
      handleNeighbours(operationGraph.successors(curr), curr, true)
      handleNeighbours(operationGraph.predecessors(curr), curr, false)

    val edges = operationGraph.edges.filterNot(toDelete) ++ toAdd
    new OperationGraph(operationGraph.root, edges)

  private def makeProcessNodePair(n: Node) =
    n match
      case s: SendNode => Some(s.caller, n)
      case r: RecvNode => Some(r.caller, n)
      case _ => None

