package deadlockFinder
package analysis.pnet

trait Node:
  val label: String = ""

class Transition(override val label: String = "") extends Node

class Place(override val label: String = "") extends Node

type Edge = (Node, Node)
