package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, IntermediateNode, MergeNode, OperationGraph, RecvNode, SendNode, SplitNode, Node as OGNode}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer, Queue, Set}

class PetriNetBuilder(operationGraph: OperationGraph):
  case class QueueEntry(from: Node, to: Node, transition: Transition)

  private val edges = ListBuffer.empty[Edge]
  private val queue = Queue.empty[(OGNode, Transition)]
  private val groups = HashMap.empty[GroupInfo, NodeGroup]
  private val groupsAnyRecv = HashMap.empty[ProcessRank, NodeGroup]
  private val transitions = HashMap.empty[OGNode, Transition]
  private val mergePlaces = HashMap.empty[MergeNode, Place]
  private val seen = Set.empty[OGNode]
  private val groupsBuilder = new NodeGroupBuilder(operationGraph)

  def build(): PetriNet =
    // Add inner edges for all groups
    edges ++= groupsBuilder.innerEdges

    val firstP = new Place
    val firstT = new Transition
    addEdge(firstP, firstT)
    queue += ((operationGraph.root, firstT))

    while queue.nonEmpty do
      val (n, t) = queue.dequeue()
      processEntry(n, t)

    new PetriNet(firstP, edges.toList)

  private def processEntry(ognode: OGNode, prevTran: Transition): Unit =
    val p = new Place
    val isSeen = seen(ognode)
    ognode match
      case curr: CallNode =>
        processCallNode(prevTran, p, curr)

      case curr: SplitNode =>
        processSplitNode(ognode, prevTran, p)

      case curr: MergeNode =>
        processMergeNode(prevTran, curr)

      case curr: IntermediateNode =>
        processIntermediateNode(prevTran, p, curr)

    seen.add(ognode)

  private def processCallNode(prevTran: Transition, p: Place, curr: CallNode): Unit =
    if !seen(curr) then
      val successors = operationGraph.successors(curr)
      addEdge(prevTran, p)
      if successors.length == 1 then
        val next = successors.head
        next match
          case n: MergeNode =>
            val nextTran = new Transition
            addEdge(p, nextTran)
            edges ++= groupsBuilder.edgesToConnectNode(curr, prevTran, nextTran)
            queue += ((next, nextTran))
          case n: IntermediateNode =>
            val nextTran = getOrCreateSharedTransition(next)
            val midT = new Transition
            val midP = new Place
            addEdge(p, midT)
            addEdge(midT, midP)
            addEdge(midP, nextTran)
            edges ++= groupsBuilder.edgesToConnectNode(curr, prevTran, midT)
            queue += ((next, nextTran))
          case _ =>
            val nextTran = getOrCreateSharedTransition(next)
            addEdge(p, nextTran)
            edges ++= groupsBuilder.edgesToConnectNode(curr, prevTran, nextTran)
            queue += ((next, nextTran))
      else if successors.length > 1 then
        assert(false, "PetriNetBuilder.processEntry: Call node has more than one successor")
      else
        assert(false, "PetriNetBuilder.processEntry: Call node has no successor")

  private def processSplitNode(ognode: OGNode, prevTran: Transition, p: Place): Unit =
    if !seen(ognode) then
      val successors = operationGraph.successors(ognode)
      addEdge(prevTran, p)
      for next <- successors do
        val nextTran = new Transition
        addEdge(p, nextTran)
        queue += ((next, nextTran))

  private def processMergeNode(prevTran: Transition, curr: MergeNode): Unit =
    val mergeP = mergePlaces.getOrElseUpdate(curr, new Place)
    addEdge(prevTran, mergeP)
    if !seen(curr) then
      for next <- operationGraph.successors(curr) do
        val nextTran = getOrCreateSharedTransition(next)
        addEdge(mergeP, nextTran)
        queue += ((next, nextTran))

  private def processIntermediateNode(prevTran: Transition, p: Place, curr: IntermediateNode): Unit =
    if !seen(curr) then
      val successors = operationGraph.successors(curr)
      if successors.isEmpty then
        addEdge(prevTran, p)
      else
        for next <- successors do
          next match
            case n: MergeNode =>
              val nextTran = new Transition
              addEdge(p, nextTran)
              queue += ((next, nextTran))
            case n: IntermediateNode =>
              val nextTran = getOrCreateSharedTransition(next)
              addEdge(prevTran, p)
              addEdge(p, nextTran)
              queue += ((next, nextTran))
            case _ =>
              val nextP = new Place
              val nextT = new Transition
              addEdge(prevTran, nextP)
              addEdge(nextP, nextT)
              queue += ((next, nextT))

  def getOrCreateSharedTransition(node: OGNode): Transition =
    transitions.get(node) match
      case Some(t) => t
      case None =>
        val t = new Transition
        transitions.put(node, t)
        t

  def addEdge(from: Node, to: Node): Unit =
    edges += ((from, to))

object PetriNetBuilder:
  def apply(operationGraph: OperationGraph): PetriNet = new PetriNetBuilder(operationGraph).build()
