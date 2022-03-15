package deadlockFinder
package analysis.pnet

class PetriNet(val root: Place, val edges: List[Edge]):
  private val map = edges.groupMap(_.from)(_.to)

  val nodes: List[Node] =
    edges.flatMap(e => List(e.from, e.to)).distinct

  def successors(node: Node): List[Node] =
    map(node)
