package deadlockFinder
package cfg

import scala.collection.mutable
import scala.annotation.tailrec

type DominanceFrontiers = Map[String, Set[String]]

type DominatorTree = Map[String, Set[String]]

class Dominators(cfg: CfgGraph):
  val immediateDoms: Map[String, String] =
    Dominators.findImmediateDominators(cfg)

  val df: DominanceFrontiers =
    Dominators.findDominanceFrontiers(cfg, immediateDoms)

  val dtree: DominatorTree =
    Dominators.computeDominatorTree(immediateDoms)

  /** Returns dominance frontier for the given node. */
  def getDominanceFrontier(node: String): Set[String] =
    df(node)

  /** Returns a set of children of the given node in dominance tree. */
  def getDomTreeChildren(node: String): Set[String] =
    dtree.getOrElse(node, Set.empty)

  /** Returns a set of nodes that are dominated by the given node. */
  def getDominatedNodes(node: String): Set[String] =
    val children = getDomTreeChildren(node)
    children.flatMap(getDominatedNodes)
      .union(children)

object Dominators:
  def apply(cfg: CfgGraph): Dominators =
    new Dominators(cfg)

  /** Returns map from a node to its immediate dominator. Implementation of an
    * algorithm by Keith D. Cooper.
    */
  def findImmediateDominators(cfg: CfgGraph): Map[String, String] =
    val Undefined = -1

    val rposta = rpostOrder(cfg).toArray

    val nodeToInd = rposta.zipWithIndex.toMap
    val doms = Array.fill(rposta.length)(Undefined)
    doms(0) = 0 // This is the entry node.
    val preds = cfg.nodes
      .map(n =>
        val preds = cfg.predecessors(n).map(nodeToInd)
        (nodeToInd(n), preds)
      )
      .toMap

    @tailrec
    def intersectPreds(newIdom: Int, preds: List[Int]): Int = preds match
      case p :: rest =>
        if newIdom == Undefined then
          // Find first processed predecessor.
          if doms(p) != Undefined then intersectPreds(p, rest)
          else intersectPreds(newIdom, rest)
        else if doms(p) != Undefined then
          intersectPreds(intersect(newIdom, p), rest)
        else
          // Predecessor is not processed, skip it.
          intersectPreds(newIdom, rest)
      case _ => newIdom

    @tailrec
    def intersect(n1: Int, n2: Int): Int =
      // "Climb" up the dominator tree.
      if n1 > n2 then intersect(doms(n1), n2)
      else if n2 > n1 then intersect(n1, doms(n2))
      else n1

    var changed = true

    while changed do
      changed = false
      // Skip node 0 which is the entry node.
      for node <- 1 until rposta.length do
        val newIdom = intersectPreds(Undefined, preds(node))
        if doms(node) != newIdom then
          doms(node) = newIdom
          changed = true

    doms.zipWithIndex
      .map((idom, node) => (rposta(node), rposta(idom)))
      .filter((n, _) => n != cfg.entry) // Entry node - no immediate dominator.
      .toMap

  /** Find dominance frontier for each node in CFG. Return a map from a node to
    * its dominance frontier.
    */
  def findDominanceFrontiers(cfg: CfgGraph): DominanceFrontiers =
    val doms = findImmediateDominators(cfg)
    findDominanceFrontiers(cfg, doms)

  def findDominanceFrontiers(
      cfg: CfgGraph,
      doms: Map[String, String]
  ): DominanceFrontiers =
    val df = mutable.Map[String, mutable.Set[String]]()
    for node <- cfg.nodes do df(node) = mutable.Set()

    val joins = cfg.nodes.filter(n => cfg.predecessors(n).length > 1)

    @tailrec
    def walkUpDoms(join: String, idom: String, curr: String): Unit =
      if curr != idom then
        df(curr).addOne(join)
        walkUpDoms(join, idom, doms(curr))

    for join <- joins do
      for pred <- cfg.predecessors(join) do
        walkUpDoms(join, doms(join), pred)

    df.map((k, v) => (k, v.toSet)).toMap

  def rpostOrder(cfg: CfgGraph): List[String] =
    val list = mutable.ListBuffer[String]()
    val visited = mutable.Set[String]()
    def dfs(curr: String): Unit =
      if visited.add(curr) then
        for succ <- cfg.successors(curr) do dfs(succ)
        list.addOne(curr)
    dfs(cfg.entry)
    list.toList.reverse

  def computeDominatorTree(doms: Map[String, String]): DominatorTree =
    doms.toSeq
      .map((k, v) => (v, k))
      .groupMap(_._1)(_._2)
      .map((k, v) => (k, v.toSet))
