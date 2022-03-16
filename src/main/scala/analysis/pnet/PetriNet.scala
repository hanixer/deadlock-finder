package deadlockFinder
package analysis.pnet

class PetriNet(val root: Place, val edges: List[Edge]):
  private val map = edges.groupMap(_.from)(_.to)
  private val reverseMap = edges.groupMap(_.to)(_.from)

  val nodes: List[Node] =
    edges.flatMap(e => List(e.from, e.to)).distinct

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
