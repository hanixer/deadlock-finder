package deadlockFinder
package translation

import cfg.{CfgGraph, DominanceFrontiers, Dominators}
import common.*
import hir.{BinaryExpr, CallExpr, Expr, SimpleExpr, UnaryExpr, Variable}
import lil.*

import scala.annotation.tailrec
import scala.collection.*
import deadlockFinder.hir.AbstractVar

object LilToSsa:
  def apply(prog: Program): Program =
    val funcs = prog.funcs.map(f => apply(f))
    prog.copy(funcs = funcs)

  def apply(func: FuncDecl): FuncDecl =
    val cfg = CfgGraph(func)
    val doms = Dominators(cfg)
    // 1. Place phi-functions (introduce parameters for blocks)
    val funcWithPhi = placePhi(func, cfg, doms)
    // 2. Rename variables
    renameVariables(funcWithPhi, doms)

  case class VarInfo(name: String, typ: Type)

  def placePhi(func: FuncDecl, cfg: CfgGraph, doms: Dominators): FuncDecl =
    val blockParams = buildBlockToParams(func, cfg, doms)
    addBlockParams(func, blockParams)

  /** Returns a map: block label -> params of that block */
  def buildBlockToParams(
      func: FuncDecl,
      cfg: CfgGraph,
      doms: Dominators
  ): Map[String, List[BlockParam]] =
    val varToDefs = buildVarToDefBlock(func)
    val varToPhiBlocks = varToDefs.map((v, d) => (v, findPhiForVar(v, d, doms)))
    val resolvedVars = resolveVars(func)
    val pairs =
      cfg.getAllNodes.map(b =>
        val vars = varToPhiBlocks
          .filter((v, phiBlocks) => phiBlocks(b))
          .map((v, _) => resolvedVars(v))
          .toList
          .sortBy(_.v.name)
        (b, vars)
      )
    pairs.toMap

  /** Return a map: variable -> blocks in which the variable is defined */
  def buildVarToDefBlock(func: FuncDecl): Map[String, List[String]] =
    def isTemp(name: String): Boolean = name.startsWith("t~")
    val pairs =
      func.body.flatMap(b =>
        b.stmts.flatMap(s =>
          s match
            case v: VarDecl if !isTemp(v.v.name) =>
              Some(v.v.name, b.label)
            case a: Assignment if !isTemp(a.lhs.name) =>
              Some(a.lhs.name, b.label)
            case _ => None
        )
      )
    pairs.groupMap(_._1)(_._2)

  def findPhiForVar(
      v: String,
      defBlocks: List[String],
      doms: Dominators
  ): Set[String] =
    @tailrec
    def iter(todo: List[String], targets: Set[String]): Set[String] =
      if todo.nonEmpty then
        val curr = todo.head
        if targets.contains(curr) then iter(todo.tail, targets)
        else
          val frontier = doms.getDominanceFrontier(curr)
          val todo1 =
            if !defBlocks.contains(curr) then todo.tail.appendedAll(frontier)
            else todo.tail
          iter(todo1, targets + curr)
      else targets
    val initial = defBlocks.flatMap(b => doms.getDominanceFrontier(b)).distinct
    iter(initial, Set())

  def resolveVars(func: FuncDecl): Map[String, BlockParam] =
    val fromBody = func.body.flatMap { b =>
      b.stmts.flatMap(s =>
        s match
          case v: VarDecl => Some((v.v.name, BlockParam(v.v, v.typ)))
          case _          => None
      )
    }
    val params = func.params.map(p => (p.name, BlockParam(p)))
    fromBody.appendedAll(params).toMap

  def addBlockParams(
      func: FuncDecl,
      blockParams: Map[String, List[BlockParam]]
  ): FuncDecl =
    def mkVars(label: String): List[Variable] =
      blockParams(label).map(p => Variable(p.v.name, p.loc))

    def transformTransfer(s: Transfer): Transfer = s match
      case j: Jump => j.copy(vars = mkVars(j.label))
      case cj: CondJump =>
        cj.copy(
          thenArgs = mkVars(cj.thenLabel),
          elseArgs = mkVars(cj.elseLabel)
        )
      case _ => s

    def transformBlock(b: Block): Block =
      val transfer = transformTransfer(b.transfer)
      val params = blockParams(b.label)
      b.copy(params = params, transfer = transfer)

    val blocks = func.body.map(transformBlock)
    func.copy(body = blocks)

  case class RenameState(
      env: Map[String, Int],
      global: mutable.Map[String, Int]
  ):
    def lookup(name: String): Int = env.getOrElse(name, 0)
    def freshVar(name: String): (Int, RenameState) =
      val index =
        if global.contains(name) then global(name) + 1
        else 0
      global(name) = index
      val s = copy(env = env + (name -> index))
      (index, s)

  def renameVariables(func: FuncDecl, doms: Dominators): FuncDecl =
    def renameAndCollect(
        block: Block,
        state: RenameState,
        acc: List[Block]
    ): List[Block] =
      // Rename variables in current block.
      val (block1, state1) = renameVariables(block, state, doms)
      // Recurr to children blocks.
      val children = doms.getDomTreeChildren(block.label).map(func.labelToBlock)
      val childrenRes = children.foldLeft(acc)((acc, childB) =>
        renameAndCollect(childB, state1, acc)
      )
      // Add current.
      block1 :: childrenRes

    val initState = RenameState(Map(), mutable.Map())
    // Order of result blocks is irrelevant, except that entry node is the first one.
    val blocks = renameAndCollect(func.body.head, initState, List.empty)
    func.copy(body = blocks)

  def renameVariables(
      block: Block,
      state: RenameState,
      doms: Dominators
  ): (Block, RenameState) =
    def handleVariable(v: AbstractVar, state: RenameState): AbstractVar =
      v match
        case v: Variable =>
          val index = state.lookup(v.name)
          SsaVariable(v.name, index, v.loc)
        case _ => v

    def handleSimpleExpr(expr: SimpleExpr, state: RenameState): SimpleExpr =
      expr match
        case v: Variable => handleVariable(v, state)
        case _           => expr

    def handleExpr(expr: Expr, state: RenameState): Expr = expr match
      case b: BinaryExpr =>
        val lhs = handleSimpleExpr(b.lhs, state)
        val rhs = handleSimpleExpr(b.rhs, state)
        b.copy(lhs = lhs, rhs = rhs)
      case u: UnaryExpr =>
        val e = handleSimpleExpr(u.e, state)
        u.copy(e = e)
      case c: CallExpr =>
        val args = c.args.map(a => handleSimpleExpr(a, state))
        c.copy(args = args)
      case s: SimpleExpr => handleSimpleExpr(s, state)
      case _             => expr

    def handleTransfer(t: Transfer, state: RenameState): Transfer = t match
      case j: Jump =>
        val vars = j.vars.map(e => handleVariable(e, state))
        j.copy(vars = vars)
      case cj: CondJump =>
        val thenArgs = cj.thenArgs.map(e => handleVariable(e, state))
        val elseArgs = cj.elseArgs.map(e => handleVariable(e, state))
        cj.copy(thenArgs = thenArgs, elseArgs = elseArgs)
      case r: Return =>
        val expr = r.expr.map(e => handleSimpleExpr(e, state))
        r.copy(expr = expr)

    def handleStmt(stmt: Stmt, state: RenameState): (Stmt, RenameState) =
      stmt match
        case vd: VarDecl =>
          val (index, state1) = state.freshVar(vd.v.name)
          val v = SsaVariable(vd.v.name, index, vd.loc)
          val rhs = vd.rhs.map(e => handleExpr(e, state1))
          (vd.copy(v = v, rhs = rhs), state1)
        case a: Assignment =>
          val (index, state1) = state.freshVar(a.lhs.name)
          val lhs = SsaVariable(a.lhs.name, index, a.loc)
          val rhs = handleExpr(a.rhs, state1)
          (a.copy(lhs = lhs, rhs = rhs), state1)
        case c: CallStmt =>
          val args = c.callExpr.args.map(e => handleSimpleExpr(e, state))
          (CallStmt(c.callExpr.copy(args = args)), state)
        case t: Transfer =>
          (handleTransfer(t, state), state)

    @tailrec
    def renameAndCollect(
        stmts: List[Stmt],
        acc: List[Stmt],
        state: RenameState
    ): (List[Stmt], RenameState) = stmts match
      case s :: rest =>
        val (s1, state1) = handleStmt(s, state)
        renameAndCollect(rest, s1 :: acc, state1)
      case _ => (stmts.reverse, state)
    val (stmts, state1) = renameAndCollect(block.stmts, List.empty, state)
    val transfer = handleTransfer(block.transfer, state1)
    (block.copy(stmts = stmts, transfer = transfer), state1)

end LilToSsa
