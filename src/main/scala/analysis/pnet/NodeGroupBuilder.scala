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

  type Gates = Map[ProcessRank, Map[ProcessRank, NodeGate]]

  private val senderReceiverPairs =
    callNodes.map { n => n match
      case s: SendNode =>
        (s.caller, ProcessRank.Concrete(s.callee))
      case r: RecvNode =>
        (r.callee, r.caller)
    }.distinct

  private def makeGates(isReceiver: Boolean): Gates =
    senderReceiverPairs.map { (s, r) =>
      val gate = NodeGate(s, r, isReceiver)
      if isReceiver then
        (r, (s, gate))
      else
        (s, (r, gate))
    }
      .groupMap(_._1)(_._2)
      .map { (k, v) => (k, v.toMap) }

  // TODO: Consider change Map to List
  private val receiverGates: Map[ProcessRank, Map[ProcessRank, NodeGate]] =
    makeGates(true)

  private val senderGates: Map[ProcessRank, Map[ProcessRank, NodeGate]] =
    makeGates(false)

  val innerEdges: List[Edge] =
    val senderGatesList = senderGates.toList
    receiverGates.toList.flatMap { (r, sm) =>
      sm.toList.flatMap { (s, gate) =>
        if s.isAnyRank then
          // For each sender,
          // that sends to this receiver,
          // create a transition and connect it to gates
          senderGatesList
            .filter { (s, rm) => rm.contains(r) && !s.isAnyRank }
            .flatMap { (s, rm ) =>
              val t = new Transition(s"Sync *: ${s.toShortString} => ${r.toShortString}")
              val sGate = rm(r)
              edgesForGroup(t, sGate, gate)
            }
        else
          val t = new Transition(s"Sync: ${s.toShortString} => ${r.toShortString}")
          val sGate = senderGates(s)(r)
          edgesForGroup(t, sGate, gate)
      }
    }

  private def edgesForGroup(t: Transition, sendGate: NodeGate, recvGate: NodeGate): List[Edge] =
    val r = List((sendGate.enter, t), (t, sendGate.exit), (recvGate.enter, t), (t, recvGate.exit))
    r

  def edgesToConnectNode(node: CallNode, prevTran: Transition, nextTran: Transition): List[Edge] =
    val gate =
      node match
        case n: SendNode =>
          senderGates(n.caller)(ProcessRank.Concrete(n.callee))
        case n: RecvNode =>
          receiverGates(n.caller)(n.callee)

    List((prevTran, gate.enter), (gate.exit, nextTran))
