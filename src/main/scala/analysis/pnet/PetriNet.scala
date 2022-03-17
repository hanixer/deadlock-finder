package deadlockFinder
package analysis.pnet

class PetriNet(val root: Place, val edges: List[Edge]):
  private val map = edges.groupMap(_._1)(_._2)
  private val reverseMap = edges.groupMap(_._2)(_._1)

  val nodes: List[Node] =
    edges.flatMap((n, m) => List(n, m)).distinct

  def successors(node: Node): List[Node] =
    map.getOrElse(node, List())

  def predecessors(node: Node): List[Node] =
    reverseMap.getOrElse(node, List())

  def transitions: List[Transition] =
    nodes.flatMap(n => n match {
      case t: Transition => Some(t)
      case _ => None
    })

  def places: List[Place] =
    nodes.flatMap(n => n match {
      case p: Place => Some(p)
      case _ => None
    })
