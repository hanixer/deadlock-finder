package deadlockFinder
package cfg

import scala.collection.mutable

object Dominators:

  /** Returns map from a node to its immediate dominator. Implementation of an
    * algorithm by Keith D. Cooper.
    */
  def findImmediateDominators(cfg: CfgGraph): Map[String, String] =
    val Undefined = -1

    val rposta = rpostOrder(cfg).toArray
    println(rposta.zipWithIndex.map((n, i) => s"$n:$i").mkString(","))

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
          // Find initial processed predecessor
          if doms(p) != Undefined then intersectPreds(p, rest)
          else intersectPreds(newIdom, rest)
        else if doms(p) != Undefined then
          // Intersect with current idom, go to next predecessor.
          intersectPreds(intersect(newIdom, p), rest)
        else intersectPreds(newIdom, rest)
      case _ => newIdom

    def intersect(n1: Int, n2: Int): Int =
      if n1 > n2 then intersect(doms(n1), n2)
      else if n2 > n1 then intersect(n1, doms(n2))
      else n1

    var changed = true

    while changed do
      changed = false
      // Skip node 0 which is the root.
      for node <- 1 until rposta.length do
        println(s"${rposta(node)}:$node")
        val newIdom = intersectPreds(Undefined, preds(node))
        if doms(node) != newIdom then
          doms(node) = newIdom
          println(
            doms.zipWithIndex.map((n, i) => s"${rposta(i)}:$n").mkString(",")
          )
          changed = true

    doms.zipWithIndex.map((idom, node) => (rposta(node), rposta(idom))).toMap

  def rpostOrder(cfg: CfgGraph): List[String] =
    val list = mutable.ListBuffer[String]()
    val visited = mutable.Set[String]()
    def dfs(curr: String): Unit =
      if visited.add(curr) then
        for succ <- cfg.getSuccs(curr) do dfs(succ)
        list.addOne(curr)
    dfs(cfg.entry)
    list.toList.reverse
