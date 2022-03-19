package deadlockFinder
package analysis.pnet

import analysis.opgraph.{CallNode, IntermediateNode, Node => OGNode, OperationGraph}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer, Queue, Set}

class PetriNetBuilder(operationGraph: OperationGraph):
  case class QueueEntry(from: Node, to: Node, transition: Transition)

  private val edges = ListBuffer.empty[Edge]
  private val queue = Queue.empty[(OGNode, Transition)]
  private val groups = HashMap.empty[GroupInfo, NodeGroup]
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
          val group = getOrCreateGroup(curr)
          if successors.length == 1 then
            val next = successors.head
            val nextTran = getOrCreateTransition(next)
            addEdge(p, nextTran)
            edges ++= group.connect(curr, prevTran, nextTran)
            queue += ((next, nextTran))
          else if successors.length > 1 then
            val currTran = getOrCreateTransition(curr)
            addEdge(p, currTran)
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

  def getOrCreateGroup(node: CallNode): NodeGroup =
    val info = GroupInfo(node)
    groups.get(info) match
      case Some(group) => group
      case None =>
        val group = NodeGroup(info)
        groups.put(info, group)
        group.innerEdges.foreach(addEdge)
        group

  def addEdge(from: Node, to: Node): Unit =
    edges += ((from, to))

object PetriNetBuilder:
  def apply(operationGraph: OperationGraph): PetriNet = new PetriNetBuilder(operationGraph).build()
