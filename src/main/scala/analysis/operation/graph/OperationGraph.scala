package deadlockFinder
package analysis.operation.graph

class OperationGraph(val root: Node, map: Map[Node, List[Node]]):
    
  def successors(node: Node): List[Node] = map(node)

  def nodes: List[Node] = map.keys.toList

  def edges: List[(Node, Node)] =
    map.toList.flatMap { p => 
      p._2.map { (p._1, _) }
    }
