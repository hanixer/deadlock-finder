package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, MemoSendNode, OperationGraph, RecvNode, SendNode}

/** Builds groups for nodes of operation graph. */
class NodeGroupBuilder(operationGraph: OperationGraph):
  type Gates = Map[ProcessRank, Map[ProcessRank, NodeGate]]
  private case class CoreTransition(t: Transition, sendGate: NodeGate, recvGate: NodeGate):
    val edges: List[Edge] =
      List((sendGate.enter, t), (t, sendGate.exit), (recvGate.enter, t), (t, recvGate.exit))

  private val allNodes = operationGraph.nodes
  private val callNodes = allNodes.collect { case n: CallNode => n }

  private val senderReceiverPairs =
    callNodes.collect {
      case n: SendNode => (n.caller, ProcessRank.Concrete(n.callee))
      case n: RecvNode => (n.callee, n.caller)
    }
      .distinct

  private def makeGates(isReceiver: Boolean): Gates =
    senderReceiverPairs
      .map { (s, r) =>
        val gate = NodeGate(s, r, isReceiver)
        if isReceiver then (r, (s, gate))
        else (s, (r, gate))
      }
      .groupMap(_._1)(_._2)
      .map { (k, v) => (k, v.toMap) }

  private val receiverGates: Gates =
    makeGates(true)

  private val senderGates: Gates =
    makeGates(false)

  private val memoSenderGates: Map[ProcessRank, NodeGate] =
    callNodes
      .collect { case n: MemoSendNode => n }
      .distinct
      .map { n =>
        val arrow = s"${n.caller.toShortString}=>Memo"
        val enter = new Place(s"S I $arrow")
        val exit = new Place(s"S O $arrow")
        val gate = NodeGate(enter, exit, n.caller, false)
        (n.caller, gate)
      }
      .toMap

  private val coreTransitions: List[CoreTransition] =
    receiverGates.toList.flatMap { (r, sm) =>
      sm.toList
        .filterNot(_._1.isAnyRank)
        .map { (s, rGate) =>
          val t = new Transition(s"${s.toShortString}=>${r.toShortString}")
          val sGate = senderGates(s)(r)
          CoreTransition(t, sGate, rGate)
        }
    }

  private val coreTransitionsRecvAny: List[CoreTransition] =
    val senderGatesList = senderGates.toList
    receiverGates.toList.flatMap { (r, sm) =>
      sm.toList
        .filter(_._1.isAnyRank)
        .flatMap { (s, rGate) =>
          senderGatesList
            .filter { (s, rm) => rm.contains(r) && !s.isAnyRank }
            .map { (s, rm) =>
              val t = new Transition(s"*${s.toShortString}=>${r.toShortString}")
              val sGate = rm(r)
              CoreTransition(t, sGate, rGate)
            }
        }
    }

  private val memoSendEdges: List[Edge] =
    memoSenderGates.toList.flatMap { (rank, memoGate: NodeGate) =>
      coreTransitionsRecvAny
        .filter(_.recvGate.rank == rank)
        .flatMap { coreTranAny =>
          val partnerRank = coreTranAny.sendGate.rank
          val memoP = new Place(s"MemoP${rank.toShortString}${partnerRank.toShortString}")
          val memoT = new Transition
          val preExitP = new Place
          val preExitT = new Transition
          val senderGate = senderGates(rank)(partnerRank)
          List(
            (coreTranAny.t, memoP),
            (memoP, memoT),
            (memoGate.enter, memoT),
            (memoT, senderGate.enter),
            (memoT, preExitP),
            (senderGate.exit, preExitT),
            (preExitP, preExitT),
            (preExitT, memoGate.exit)
          )
        }
    }

  val innerEdges: List[Edge] =
    val senderGatesList = senderGates.toList
    val edges1 = coreTransitions.flatMap { coreTran => coreTran.edges }
    val edges2 = coreTransitionsRecvAny.flatMap { coreTran => coreTran.edges }
    edges1 ++ edges2 ++ memoSendEdges

  def edgesToConnectNode(node: CallNode, prevTran: Transition, nextTran: Transition): List[Edge] =
    val gate = node match
      case n: SendNode     => senderGates(n.caller)(ProcessRank.Concrete(n.callee))
      case n: RecvNode     => receiverGates(n.caller)(n.callee)
      case n: MemoSendNode => memoSenderGates(n.caller)
    List((prevTran, gate.enter), (gate.exit, nextTran))
