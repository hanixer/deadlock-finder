package deadlockFinder
package analysis.pnet

import analysis.ProcessRank

import deadlockFinder.analysis.opgraph.{CallNode, RecvNode, SendNode}

case class GroupInfo(sender: ProcessRank, receiver: ProcessRank)

object GroupInfo:
  /** Creates GroupInfo from call node. */
  def apply(node: CallNode): GroupInfo = node match
    case n: SendNode =>
      GroupInfo(n.sender, ProcessRank.Concrete(n.receiver))
    case n: RecvNode =>
      GroupInfo(n.receiver, n.sender)

class NodeGroup:
  val sendEnter = new Place
  val sendExit = new Place
  val recvEnter = new Place
  val recvExit = new Place
  val transition = new Transition

  def innerEdges: List[Edge] = List(
    (sendEnter, transition),
    (recvEnter, transition),
    (transition, sendExit),
    (transition, recvExit)
  )

  def connect(node: CallNode, prevTran: Transition, nextTran: Transition): List[Edge] = node match
    case n: SendNode =>
      List((prevTran, sendEnter), (sendExit, nextTran))
    case n: RecvNode =>
      List((prevTran, recvEnter), (recvExit, nextTran))
