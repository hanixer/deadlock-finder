package deadlockFinder
package cfg

import scala.collection.mutable

object Dominators:

  /**
   * Returns map from a node to its immediate dominator.
   * Implementation of an algorithm by Keith D. Cooper.
   */
  def findImmediateDominators(cfg: CfgGraph): Map[String, String] =
    val Undefined = ""
    val nodes = cfg.getAllNodes

    val doms = mutable.Map[String, String]()
    for n <- nodes do
      doms(n) = Undefined
    doms(cfg.entry) = cfg.entry

    val rpost = rpostOrder(cfg)
    var changed = true

    def splitPreds(node: String): (String, List[String]) =
      def iter(preds: List[String], acc: List[String]): (String, List[String]) =
        if preds.isEmpty then
          throw new Error(s"At least one predecessor must have been processed for node $node")
        else if doms(preds.head) != Undefined then
          (preds.head, preds.tail ++ acc)
        else
          iter(preds.tail, preds.head :: acc)
      iter(cfg.getPreds(node), List())

    def intersectPreds(newIdom: String, preds: List[String]): String = preds match
      case p :: rest => 
        if newIdom == Undefined then 
          // Find initial processed predecessor
          if doms(p) != Undefined then intersectPreds(doms(p), rest)
          else intersectPreds(newIdom, rest)
        else
          ???

      case _ => newIdom

    while changed do
      changed = false
      for node <- rpost if node != cfg.entry do        
        val preds = cfg.getPreds(node)
        preds
        ???

    Map.empty

    
  def rpostOrder(cfg: CfgGraph): List[String] =
    val list = mutable.ListBuffer[String]()
    val visited = mutable.Set[String]()
    def dfs(curr: String): Unit = 
      if visited.add(curr) then
        for succ <- cfg.getSuccs(curr) do          
          dfs(succ)
        list.addOne(curr)
    dfs(cfg.entry)
    list.toList.reverse