package deadlockFinder
package translation

import cfg.{CfgGraph, DominanceFrontiers, Dominators}
import common.*
import expr.*
import lil.*

import deadlockFinder.analysis.Liveness

import scala.annotation.tailrec
import scala.collection.*

class LilToSsa(initialFunc: FuncDecl):
  private val cfg = CfgGraph(initialFunc)
  private val doms = Dominators(cfg)
  private val liveness = new Liveness(initialFunc)

  def transform(): FuncDecl =
    // 1. Place phi-functions (introduce parameters for blocks)
    val funcWithPhi = placePhi(initialFunc)
    // 2. Rename variables
    renameVariables(funcWithPhi)

  def placePhi(func: FuncDecl): FuncDecl =
    val blockParams = buildBlockToParams(func)
    addBlockParams(func, blockParams)

  def buildBlockToParams(func: FuncDecl): Map[String, List[BlockParam]] =
    val varToDefs = buildVarToDefBlock(func)
    val varToPhiBlocks = varToDefs.map((v, d) => (v, findPhiForVar(v, d)))
    val resolvedVars = resolveVars(func)
    val pairs =
      cfg.nodes.map(b =>
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
      func.body.flatMap { b =>
        b.stmts.flatMap { s =>
          s match
            case a: Assignment =>
              Some(a.lhs.name, b.label)
            case _ => None
        }
      }
    pairs.groupMap(_._1)(_._2)

  /** Find all blocks in which the specified must be added as a phi parameter. */
  def findPhiForVar(v: String, defBlocks: List[String]): Set[String] =
    @tailrec
    def iter(todo: List[String], targets: Set[String], seen: Set[String]): Set[String] =
      if todo.nonEmpty then
        val curr = todo.head
        if seen.contains(curr) then iter(todo.tail, targets, seen)
        else
          val frontier = doms.getDominanceFrontier(curr)
          val todo1 =
            if !defBlocks.contains(curr) then todo.tail.appendedAll(frontier)
            else todo.tail
          val targets1 =
            if liveness.isVariableLiveInBlock(v, curr) then targets + curr
            else targets
          iter(todo1, targets1, seen + curr)
      else targets
    val initial = defBlocks.flatMap(b => doms.getDominanceFrontier(b)).distinct
    iter(initial, Set.empty, Set.empty)

  /** Make a map from a variable name to its declaration info (type, etc.) */
  def resolveVars(func: FuncDecl): Map[String, BlockParam] =
    val declarations = func.declarations.map { v => (v.v.name, BlockParam(v.v, v.typ)) }
    val params = func.params.map(p => (p.name, BlockParam(p)))
    declarations.appendedAll(params).toMap

  /** Transform a function by adding arguments to jumps and adding parameters to blocks
    */
  def addBlockParams(func: FuncDecl, blockParams: Map[String, List[BlockParam]]): FuncDecl =
    def mkVars(label: String): List[Variable] =
      blockParams(label).map(p => Variable(p.v.name, p.loc))

    def transformTransfer(s: Transfer): Transfer = s match
      case j: Jump => j.copy(vars = mkVars(j.label))
      case cj: CondJump =>
        val ta = mkVars(cj.thenLabel)
        val ea = mkVars(cj.elseLabel)
        cj.copy(thenArgs = ta, elseArgs = ea)
      case _ => s

    def transformBlock(b: Block): Block =
      val transfer = transformTransfer(b.transfer)
      val params = blockParams(b.label)
      b.copy(params = params, transfer = transfer)

    val blocks = func.body.map(transformBlock)
    func.copy(body = blocks)

  /** State for renaming - an immutable environment, and a mutable global map */
  case class RenameState(env: Map[String, Int], global: mutable.Map[String, Int]):
    def lookup(name: String): Option[Int] = env.get(name)
    def freshVar(v: AbstractVar): (SsaVariable, RenameState) =
      val name = v.name
      val index =
        if global.contains(name) then global(name) + 1
        else 0
      global(name) = index
      val ssaVar = SsaVariable(v.name, index, v.loc)
      val s = copy(env = env + (name -> index))
      (ssaVar, s)

  /** Rename variables in a function declaration */
  def renameVariables(func: FuncDecl): FuncDecl =
    def renameAndCollect(
        block: Block,
        state: RenameState,
        acc: List[Block]
    ): List[Block] =
      // Rename variables in current block.
      val (block1, state1) = renameVariables(block, state)
      // Recur into children blocks.
      val children = doms.getDomTreeChildren(block.label).map(func.labelToBlock)
      val childrenRes = children.foldLeft(acc)((acc, childB) => renameAndCollect(childB, state1, acc))
      // Add current.
      block1 :: childrenRes

    val initState = RenameState(Map(), mutable.Map())
    // Order of result blocks is irrelevant, except that entry node is the first one.
    val blocks = renameAndCollect(func.body.head, initState, List.empty)
    func.copy(body = blocks)

  /** Rename variables in a block. Returns a transformed block and rename state
    */
  def renameVariables(block: Block, state: RenameState): (Block, RenameState) =
    def handleVariable(v: AbstractVar, state: RenameState): AbstractVar =
      v match
        case v: Variable =>
          state.lookup(v.name) match
            case Some(index) => SsaVariable(v.name, index, v.loc)
            case _           => v
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
        val cond = handleSimpleExpr(cj.cond, state)
        val thenArgs = cj.thenArgs.map(e => handleVariable(e, state))
        val elseArgs = cj.elseArgs.map(e => handleVariable(e, state))
        cj.copy(cond = cond, thenArgs = thenArgs, elseArgs = elseArgs)
      case r: Return =>
        val expr = r.expr.map(e => handleSimpleExpr(e, state))
        r.copy(expr = expr)

    def handleStmt(stmt: Stmt, state: RenameState): (Stmt, RenameState) =
      stmt match
        case a: Assignment =>
          val (lhs, state1) = state.freshVar(a.lhs)
          val rhs = handleExpr(a.rhs, state)
          (a.copy(lhs = lhs, rhs = rhs), state1)
        case c: CallStmt =>
          val args = c.callExpr.args.map(e => handleSimpleExpr(e, state))
          (CallStmt(c.callExpr.copy(args = args)), state)
        case t: Transfer =>
          (handleTransfer(t, state), state)
        case a: Assert =>
          val expr = handleExpr(a.expr, state)
          (a.copy(expr = expr), state)
        case _ => (stmt, state)

    // Process block params.
    val (params, state1) =
      block.params.foldRight((List.empty[BlockParam], state))((p, acc) =>
        val (ps, state) = acc
        val (v, state1) = state.freshVar(p.v)
        (p.copy(v = v) :: ps, state1)
      )
    // Process statements.
    val (stmts, state2) =
      block.stmts.foldLeft((List.empty[Stmt], state1))((acc, stmt) =>
        val (stmts, state) = acc
        val (stmt1, state1) = handleStmt(stmt, state)
        (stmt1 :: stmts, state1)
      )
    // Process transfer statement.
    val transfer = handleTransfer(block.transfer, state2)
    val block1 =
      block.copy(stmts = stmts.reverse, transfer = transfer, params = params)
    (block1, state2)

object LilToSsa:
  def apply(prog: Program): Program =
    val funcs = prog.funcs.map(f => apply(f))
    prog.copy(funcs = funcs)

  def apply(func: FuncDecl): FuncDecl = new LilToSsa(func).transform()
