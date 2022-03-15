package deadlockFinder
package analysis.pnet

trait Node

class Transition extends Node

class Place extends Node

trait Edge:
  def from: Node
  def to: Node

case class TPEdge(t: Transition, p: Place) extends Edge:
  override val from: Node = t
  override val to: Node = p

case class PTEdge(p: Place, t: Transition) extends Edge:
  override val from: Node = p
  override val to: Node = t
