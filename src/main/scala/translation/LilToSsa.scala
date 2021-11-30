package deadlockFinder
package translation

import lil.*
import common.*
import cfg.CfgGraph
import scala.collection.*
import deadlockFinder.cfg.Dominators
import deadlockFinder.cfg.DominanceFrontiers
import scala.annotation.tailrec

object LilToSsa:
  def apply(prog: Program): Program =
    ???

  def apply(func: FuncDecl): FuncDecl =
    val cfg = CfgGraph(func)
    // 1. Place phi-functions (introduce parameters for blocks)
    // 2. Rename variables
    ???

  case class VarInfo(name: String, typ: Type)

  def placePhi(func: FuncDecl, cfg: CfgGraph): FuncDecl =

    // Build map: block -> params of the block
    val blockParams = buildBlockToParams(func, cfg)

    ???

  def resolveVars(func: FuncDecl): Map[String, Param] =
    val fromBody = func.body.flatMap(b =>
      b.stmts.flatMap(s =>
        s match
          case v: VarDecl => Some((v.name, Param(v.name, v.t, v.loc)))
          case _          => None
      )
    )
    val params = func.params.map(p => (p.name, p))
    fromBody.appendedAll(params).toMap

  /** Return a map: variable -> blocks in which the variable is defined */
  def buildVarToDefBlock(func: FuncDecl): Map[String, List[String]] =
    val pairs =
      func.body.flatMap(b =>
        b.stmts.flatMap(s =>
          s match
            case v: VarDecl    => Some(v.name, b.label)
            case a: Assignment => Some(a.lhs, b.label)
            case _             => None
        )
      )
    pairs.groupMap(_._1)(_._2)

  /** Returns a map: block label -> params of that block */
  def buildBlockToParams(
      func: FuncDecl,
      cfg: CfgGraph
  ): Map[String, List[Param]] =
    val varToDefs = buildVarToDefBlock(func)
    val df = Dominators.findDominanceFrontiers(cfg)
    println(df)
    val varToPhiBlocks = varToDefs.map((v, d) => (v, findPhiForVar(v, d, df)))
    val resolvedVars = resolveVars(func)
    val pairs =
      cfg.getAllNodes.map(b =>
        val vars = varToPhiBlocks
          .filter((v, phiBlocks) => phiBlocks(b))
          .map((v, _) => resolvedVars(v))
          .toList
          .sortBy(_.name)
        (b, vars)
      )
    pairs.toMap

  def findPhiForVar(
      v: String,
      defBlocks: List[String],
      df: DominanceFrontiers
  ): Set[String] =
    @tailrec
    def iter(todo: List[String], targets: Set[String]): Set[String] =
      println(s"todo: ${todo.mkString(",")}; targets: ${targets.mkString(",")}")

      if !todo.isEmpty then
        val curr = todo.head
        if targets.contains(curr) then
          iter(todo.tail, targets)
        else
          val frontier = df(curr)
          val todo1 =
            if !defBlocks.contains(curr) then todo.tail.appendedAll(frontier)
            else todo.tail
          iter(todo1, targets + curr)
      else targets
    println("=========")
    println(v)
    val initial = defBlocks.flatMap(b => df(b)).toSet.toList
    iter(initial, Set())

end LilToSsa
