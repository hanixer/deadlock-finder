package deadlockFinder
package analysis.pnet

trait Node

class Transition extends Node

class Place extends Node

type Edge = (Node, Node)
