package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, IntermediateNode, OperationGraph, RecvNode, SendNode, Node as OGNode}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer, Queue, Set}

class PetriNetBuilder(operationGraph: OperationGraph):
  case class QueueEntry(from: Node, to: Node, transition: Transition)

  private val edges = ListBuffer.empty[Edge]
  private val queue = Queue.empty[(OGNode, Transition)]
  private val groups = HashMap.empty[GroupInfo, NodeGroup]
  private val groupsAnyRecv = HashMap.empty[ProcessRank, NodeGroup]
  private val transitions = HashMap.empty[OGNode, Transition]
  private val seen = Set.empty[OGNode]

  def build(): PetriNet =
    val firstP = new Place
    val firstT = new Transition
    addEdge(firstP, firstT)
    queue += ((operationGraph.root, firstT))

    while queue.nonEmpty do
      val (n, t) = queue.dequeue()
      processEntry(n, t)

    new PetriNet(firstP, edges.toList)

  private def processEntry(ognode: OGNode, prevTran: Transition): Unit =
    if seen.add(ognode) then
      val successors = operationGraph.successors(ognode)
      val p = new Place
      ognode match
        case curr: CallNode =>
          addEdge(prevTran, p)
          val groups = getOrCreateGroups(curr)
          if successors.length == 1 then
            val next = successors.head
            val nextTran = getOrCreateTransition(next)
            addEdge(p, nextTran)
            for group <- groups do
              edges ++= group.connect(curr, prevTran, nextTran)
              queue += ((next, nextTran))
          else if successors.length > 1 then
            val currTran = getOrCreateTransition(curr)
            addEdge(p, currTran)
            for group <- groups do
              edges ++= group.connect(curr, prevTran, currTran)
              operationGraph.successors(curr).foreach { next =>
                queue += ((next, currTran))
              }
          else
            assert(false, "PetriNetBuilder.processEntry: Call node has no successor")

        case curr: IntermediateNode =>
          if successors.isEmpty then
            addEdge(prevTran, p)
          else
            successors.foreach { next =>
              next match
                case nexti: IntermediateNode =>
                  val nextTran = getOrCreateTransition(next)
                  addEdge(prevTran, p)
                  addEdge(p, nextTran)
                  queue += ((next, nextTran))
                case _ =>
                  queue += ((next, prevTran))
            }

  def getOrCreateTransition(node: OGNode): Transition =
    transitions.get(node) match
      case Some(t) => t
      case None =>
        val t = new Transition
        transitions.put(node, t)
        t

  def getOrCreateGroups(node: CallNode): List[NodeGroup] =
    node match
      case n: SendNode =>
        val groupAny = getOrCreateGroupAnyRecv(ProcessRank.Concrete(n.receiver))
        val info = GroupInfo(node)
        groups.get(info) match
          case Some(group) =>
            List(group, groupAny)
          case None =>
            val group = new NodeGroup(info)
            group.innerEdges.foreach(addEdge)
            groups.put(info, group)
            List(group, groupAny)
      case n: RecvNode =>
        n.receiver match
          case s: ProcessRank.Concrete =>
            val info = GroupInfo(node)
            groups.get(info) match
              case Some(group) => List(group)
              case None =>
                val group = new NodeGroup(info)
                group.innerEdges.foreach(addEdge)
                groups.put(info, group)
                List(group)
          case ProcessRank.AnyRank =>
            val group = getOrCreateGroupAnyRecv(n.sender)
            List(group)

  def getOrCreateGroupAnyRecv(sender: ProcessRank): NodeGroup =
    groupsAnyRecv.get(sender) match
      case Some(group) => group
      case None =>
        val info = GroupInfo(ProcessRank.AnyRank, ProcessRank.AnyRank) // TODO
        val group = new NodeGroup(info)
        group.innerEdges.foreach(addEdge)
        groupsAnyRecv.put(sender, group)
        group

  def addEdge(from: Node, to: Node): Unit =
    edges += ((from, to))

object PetriNetBuilder:
  def apply(operationGraph: OperationGraph): PetriNet = new PetriNetBuilder(operationGraph).build()
