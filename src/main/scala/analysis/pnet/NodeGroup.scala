package deadlockFinder
package analysis.pnet

import analysis.ProcessRank

import deadlockFinder.analysis.opgraph.{CallNode, RecvNode, SendNode}

case class GroupInfo(sender: ProcessRank, receiver: ProcessRank)

object GroupInfo:
  /** Creates GroupInfo from call node. */
  def apply(node: CallNode): GroupInfo = node match
    case n: SendNode =>
      GroupInfo(n.caller, ProcessRank.Concrete(n.callee))
    case n: RecvNode =>
      GroupInfo(n.callee, n.caller)

class NodeGroup(val info: GroupInfo):
  private val senderS = info.sender.toShortString
  private val receiverS = info.receiver.toShortString
  private val arrow = s"$senderS => $receiverS"

  val sendEnter = new Place(s"Enter sndr $arrow")
  val sendExit = new Place(s"Exit sndr $arrow")
  val recvEnter = new Place(s"Enter rcvr $arrow")
  val recvExit = new Place(s"Exit rcvr $arrow")
  val transition = new Transition(s"Sync: $arrow")

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
