package deadlockFinder
package analysis.opgraph

import common.Graph

class OperationGraph(val root: Node, edges: List[Edge]) extends Graph[Node](edges):
  val exit: Node =
    if edges.isEmpty then
      root
    else
      val candidates = nodes.filter(n => predecessors(n).lengthCompare(0) > 0 && successors(n).isEmpty)
      assert(candidates.lengthCompare(1) == 0, "OperationGraph: there should be exactly one exit one")
      candidates.head
