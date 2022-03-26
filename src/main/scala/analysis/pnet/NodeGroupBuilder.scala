package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, OperationGraph, RecvNode, SendNode}

/** Builds groups for nodes of operation graph. */
class NodeGroupBuilder(operationGraph: OperationGraph):
  private val allNodes = operationGraph.nodes

  private val callNodes = allNodes.collect { case n: CallNode => n }

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
              val t = new Transition(s"*${s.toShortString}=>${r.toShortString}")
              val sGate = rm(r)
              edgesForGroup(t, sGate, gate)
            }
        else
          val t = new Transition(s"${s.toShortString}=>${r.toShortString}")
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
