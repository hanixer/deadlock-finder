package deadlockFinder
package analysis.pnet

import common.Graph

class PetriNet(val root: Place, edges: List[Edge]) extends Graph[Node](edges):
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
