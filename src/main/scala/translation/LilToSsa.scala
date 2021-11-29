package deadlockFinder
package translation

import lil.*
import cfg.CfgGraph
import scala.collection.*
import deadlockFinder.cfg.Dominators

object LilToSsa:
  def apply(prog: Program): Program =
    ???

  def apply(func: FuncDecl): FuncDecl =
    val cfg = CfgGraph(func)
    // 1. Place phi-functions (introduce parameters for blocks)
    // 2. Rename variables
    ???

  def placePhi(func: FuncDecl, cfg: CfgGraph): FuncDecl =
    
    // Build map: variable -> blocks in which the variable is defined
    val varToDefs = func.body.flatMap(b => b.stmts.flatMap(s => s match
      case v: VarDecl => Some(v.name, b.label)
      case a: Assignment => Some(a.lhs, b.label)
      case _ => None)).groupBy(_._1)

    // Build map: block -> params of the block
    val blockParams = mutable.Map[String, mutable.Set[String]]()
    val df = Dominators.findDominanceFrontiers(cfg)
    for (v, blocks) <- varToDefs do
      val b = blocks
      def iter(todo: Set[String], done: Set[String]): Unit =
        if !todo.isEmpty then          
          ???
      ???

    
    ???

  def add[A, B](map: mutable.Map[A, mutable.Set[B]], k: A, v: B): Unit =
    if !map.contains(k) then map.put(k, mutable.Set(v))
    else map(k).addOne(v)  

end LilToSsa