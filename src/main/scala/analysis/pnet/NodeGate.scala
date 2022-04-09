package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, RecvNode, SendNode}

case class NodeGate(enter: Place, exit: Place, rank: ProcessRank, isReceiver: Boolean)

object NodeGate:
  def apply(sender: ProcessRank, receiver: ProcessRank, isReceiver: Boolean): NodeGate =
    val arrow = s"${sender.toShortString}=>${receiver.toShortString}"
    val label = if isReceiver then "R" else "S"
    val enter = new Place(s"$label I $arrow")
    val exit = new Place(s"$label O $arrow")
    val rank = if isReceiver then receiver else sender
    NodeGate(enter, exit, rank, isReceiver)
