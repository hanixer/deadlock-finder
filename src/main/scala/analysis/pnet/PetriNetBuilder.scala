package deadlockFinder
package analysis.pnet

import analysis.ProcessRank
import analysis.opgraph.{CallNode, IntermediateNode, OperationGraph, RecvNode, SendNode, Node as OGNode}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer, Queue, Set}

class PetriNetBuilder(operationGraph: OperationGraph, verbose: Boolean = true):
  case class QueueEntry(from: Node, to: Node, transition: Transition)

  private val edges = ListBuffer.empty[Edge]
  private val queue = Queue.empty[(OGNode, Transition)]
  private val groups = HashMap.empty[GroupInfo, NodeGroup]
  private val groupsAnyRecv = HashMap.empty[ProcessRank, NodeGroup]
  private val transitions = HashMap.empty[OGNode, Transition]
  private val seen = Set.empty[OGNode]
  private val groupsBuilder = new NodeGroupBuilder(operationGraph)
  private val nodeToGroups = groupsBuilder.nodeToGroups
  private val allGroups = groupsBuilder.allGroups

  def build(): PetriNet =
    // Add inner edges for all groups
    for group <- allGroups do
      edges ++= group.innerEdges

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
          val groups = getGroups(curr)
          if successors.length == 1 then
            val next = successors.head
            val nextTran = getOrCreateTransition(next)
            addEdge(p, nextTran)
            for group <- groups do
              edges ++= group.connect(curr, prevTran, nextTran)
            queue += ((next, nextTran))
          else if successors.length > 1 then
            val nextTran = new Transition
            addEdge(p, nextTran)
            for group <- groups do
              edges ++= group.connect(curr, prevTran, nextTran)
            operationGraph.successors(curr).foreach { next =>
              queue += ((next, nextTran))
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

  def getGroups(node: CallNode): List[NodeGroup] = nodeToGroups(node)

  def addEdge(from: Node, to: Node): Unit =
    edges += ((from, to))

object PetriNetBuilder:
  def apply(operationGraph: OperationGraph, verbose: Boolean = true): PetriNet = new PetriNetBuilder(operationGraph).build()
