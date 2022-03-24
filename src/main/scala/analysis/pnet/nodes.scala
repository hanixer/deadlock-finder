package deadlockFinder
package analysis.pnet

trait Node:
  val label: String = ""

class Transition(override val label: String = "") extends Node:
  override def toString: String = s"Transition $label"

class Place(override val label: String = "") extends Node:
  override def toString: String = s"Place $label"

type Edge = (Node, Node)
