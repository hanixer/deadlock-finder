package deadlockFinder
package common

/**
  * Base class for graphs.
  * 
  * Edge is represented by a pair of nodes
  * @param edges list of edges
  * @tparam N type of node
  */
class Graph[N](val edges: List[(N, N)]):
  protected val map: Map[N, List[N]] = edges.groupMap(_._1)(_._2)
  protected val reverseMap: Map[N, List[N]] = edges.groupMap(_._2)(_._1)

  val nodes: List[N] =
    edges.flatMap((n, m) => List(n, m)).distinct

  def successors(node: N): List[N] =
    map.getOrElse(node, List())

  def predecessors(node: N): List[N] =
    reverseMap.getOrElse(node, List())
