package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, OperationGraph, RecvNode, SendNode}

/** Builds groups for nodes of operation graph. */
class NodeGroupBuilder(operationGraph: OperationGraph):
  private val allNodes = operationGraph.nodes

  private val receiverSenderToGroups: Map[ProcessRank, Map[ProcessRank, NodeGroup]] =
    allNodes.collect { case n: RecvNode => n }
      .map { n => (n.caller, (n.callee, new NodeGroup(GroupInfo(n)))) }
      .groupMap(_._1)(_._2)
      .map { (k, v) => (k, v.toMap) }
    
  private val noReceiverGroups: Map[ProcessRank, NodeGroup] =
    allNodes.collect { case n: SendNode => n }
      .map(GroupInfo.apply)
      .filter { info => !receiverSenderToGroups.contains(info.receiver) }
      .map { info => (info.sender, new NodeGroup(info)) }
      .toMap

  private val callNodes = allNodes.collect { case n: CallNode => n }

  private val groupsRecvConcrete =
    callNodes.map { n => GroupInfo(n) }
      .filter( info => !info.receiver.isAnyRank )
      .map { info => (info, new NodeGroup(info)) }
      .toMap
    
  private def getList(n: CallNode): List[NodeGroup] = n match
    case s: SendNode =>
      val info = GroupInfo(n)
      val recvRank = ProcessRank.Concrete(s.callee)
      receiverSenderToGroups.get(recvRank) match
        case Some(senderToGroups) =>
          List(senderToGroups.get(s.caller), senderToGroups.get(ProcessRank.AnyRank)).flatten
        case None =>
          List(noReceiverGroups(s.caller))
    case r: RecvNode =>
      List(receiverSenderToGroups(r.caller)(r.callee))

  val nodeToGroups: Map[CallNode, List[NodeGroup]] =
    callNodes.map { n =>  (n, getList(n)) }.toMap

  val allGroups: List[NodeGroup] =
    nodeToGroups.toList.flatMap(_._2).distinct
