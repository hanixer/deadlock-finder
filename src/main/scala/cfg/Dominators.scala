package deadlockFinder
package cfg

import scala.collection.mutable

type DominanceFrontiers = Map[String, Set[String]]
type DominatorTree = Map[String, Set[String]]

class Dominators(cfg: CfgGraph):
  val immediateDoms: Map[String, String] =
    Dominators.findImmediateDominators(cfg)
  val df: DominanceFrontiers =
    Dominators.findDominanceFrontiers(cfg, immediateDoms)

object Dominators:

  /** Returns map from a node to its immediate dominator. Implementation of an
    * algorithm by Keith D. Cooper.
    */
  def findImmediateDominators(cfg: CfgGraph): Map[String, String] =
    val Undefined = -1

    val rposta = rpostOrder(cfg).toArray

    val nodeToInd = rposta.zipWithIndex.toMap
    val doms = Array.fill(rposta.length)(Undefined)
    doms(0) = 0 // This is the entry node.
    val preds = cfg.getAllNodes
      .map(n =>
        val preds = cfg.getPreds(n).map(nodeToInd)
        (nodeToInd(n), preds)
      )
      .toMap

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

    doms.zipWithIndex.map((idom, node) => (rposta(node), rposta(idom))).toMap

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
    for node <- cfg.getAllNodes do df(node) = mutable.Set()

    val doms = findImmediateDominators(cfg)
    val joins = cfg.getAllNodes.filter(n => cfg.getPreds(n).length > 1)

    def walkUpDoms(join: String, idom: String, curr: String): Unit =
      if curr != idom then
        df(curr).addOne(join)
        walkUpDoms(join, idom, doms(curr))

    for join <- joins do
      for pred <- cfg.getPreds(join) do walkUpDoms(join, doms(join), pred)

    df.map((k, v) => (k, v.toSet)).toMap

  def rpostOrder(cfg: CfgGraph): List[String] =
    val list = mutable.ListBuffer[String]()
    val visited = mutable.Set[String]()
    def dfs(curr: String): Unit =
      if visited.add(curr) then
        for succ <- cfg.getSuccs(curr) do dfs(succ)
        list.addOne(curr)
    dfs(cfg.entry)
    list.toList.reverse
