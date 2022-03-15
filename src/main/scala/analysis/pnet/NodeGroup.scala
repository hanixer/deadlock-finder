package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.operation.graph.{CallNode, RecvNode, SendNode}

case class GroupInfo(sender: ProcessRank, receiver: ProcessRank)

object GroupInfo:
  /** Creates GroupInfo from call node. */
  def apply(node: CallNode): GroupInfo = node match
    case n: SendNode =>
      GroupInfo(n.sender, ProcessRank.Concrete(n.receiver))
    case n: RecvNode =>
      GroupInfo(ProcessRank.Concrete(n.receiver), n.sender)

class NodeGroup(val info: GroupInfo):
  val sendEnter = new Place
  val sendExit = new Place
  val recvEnter = new Place
  val recvExit = new Place
  val transition = new Transition

  def innerEdges: List[Edge] = List(
    PTEdge(sendEnter, transition),
    PTEdge(recvEnter, transition),
    TPEdge(transition, sendExit),
    TPEdge(transition, recvExit)
  )

  def connect(node: CallNode, prevTran: Transition, nextTran: Transition): List[Edge] = node match
    case n: SendNode =>
      List(TPEdge(prevTran, sendEnter), PTEdge(sendExit, nextTran))
    case n: RecvNode =>
      List(TPEdge(prevTran, recvEnter), PTEdge(recvExit, nextTran))
