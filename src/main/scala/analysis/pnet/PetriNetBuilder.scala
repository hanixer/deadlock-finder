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
      ognode match
        case curr: CallNode =>
          val p = new Place
          val group = getOrCreateGroup(curr)
          operationGraph.successors(curr).foreach { next =>
            val nextTran = getOrCreateTransition(next)

            addEdge(prevTran, p)
            addEdge(p, nextTran)
            edges ++= group.connect(curr, prevTran, nextTran)

            queue += ((next, nextTran))
          }

        case curr: IntermediateNode =>
          val p = new Place
          val successors = operationGraph.successors(curr)
          if successors.isEmpty then
            val lastP = new Place
            addEdge(prevTran, lastP)
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
