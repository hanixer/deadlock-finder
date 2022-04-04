package deadlockFinder
package translation

import common.{BooleanType, VoidType}
import expr.*
import lil.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

class HilToLil:
  private val blockStmts: ListBuffer[Stmt] = ListBuffer()
  private val blocks: ListBuffer[Block] = ListBuffer()
  private var labelCount: Int = 0
  private var currLabel: String = mkLabel()
  private var isBlockFinished: Boolean = false
  private val loopStack: mutable.Stack[(String, String)] =
    mutable.Stack() // Stack of (loopStart, loopEnd) pairs

  def translateFunc(func: hil.FuncDecl): FuncDecl =
    translateStmt(func.body, "end")
    addEndBlock(func)
    val entry = Block("entry", List.empty, Jump(blocks.head.label, func.loc), func.loc)
    blocks.prepend(entry)
    FuncDecl(func.name, func.params, func.retTyp, blocks.toList, "entry", "end", func.loc)

  def addEndBlock(func: hil.FuncDecl): Unit =
    startBlock("end")
    val loc = func.loc
    if func.retTyp != VoidType then
      finishBlock(Return(Some(Variable("~retVal", loc)), loc))
    else finishBlock(Return(None, loc))

  def translateStmt(stmt: hil.Stmt, next: String, noFinalJump: Boolean = false): Unit = stmt match
    case b: hil.Block =>
      translateBlock(b, next, noFinalJump)

    case i: hil.IfThenElse =>
      val thenLabel = mkLabel()
      val elseLabel = if i.elseBlock.isDefined then mkLabel() else next
      finishBlock(CondJump(i.cond, thenLabel, elseLabel, i.loc))
      // Then branch
      startBlock(thenLabel)
      translateStmt(i.thenBlock, next)

      // Else branch, if present
      if i.elseBlock.isDefined then
        startBlock(elseLabel)
        translateStmt(i.elseBlock.get, next)

    case l: hil.Loop =>
      val isNewBlock = blockStmts.nonEmpty
      val startLabel = if isNewBlock then mkLabel() else currLabel
      loopStack.push((startLabel, next))

      if isNewBlock then
        finishBlock(Jump(startLabel, l.loc))
        startBlock(startLabel)

      translateStmt(l.body, startLabel)
      loopStack.pop()

    case wl: hil.WhileLoop =>
      val isNewBlock = blockStmts.nonEmpty
      val startLabel = if isNewBlock then mkLabel() else currLabel
      val bodyLabel = mkLabel()
      loopStack.push((startLabel, next))

      if isNewBlock then
        finishBlock(Jump(startLabel, wl.loc))
        startBlock(startLabel)

      // Condition
      translateStmt(wl.condBlock, startLabel, true)
      val condLoc = wl.condition.loc
      val condJump = CondJump(wl.condition, bodyLabel, next, condLoc)
      finishBlock(condJump)

      // Body
      startBlock(bodyLabel)
      translateStmt(wl.body, startLabel)
      loopStack.pop()

    case v: hil.VarDecl =>
      addStmt(VarDecl(Variable(v.name, v.loc), v.t, v.rhs, v.loc))

    case a: hil.Assignment =>
      addStmt(Assignment(Variable(a.lhs, a.loc), a.rhs, a.loc))

    case b: hil.Break =>
      if loopStack.isEmpty then
        throw new Error(s"break is used without loop at ${b.loc}")
      val label = loopStack.top._2
      finishBlock(Jump(label, b.loc))

    case c: hil.Continue =>
      if loopStack.isEmpty then
        throw new Error(s"continue is used without loop at ${c.loc}")
      val label = loopStack.top._1
      finishBlock(Jump(label, c.loc))

    case c: hil.CallStmt =>
      addStmt(CallStmt(c.callExpr))

    case a: hil.Assert =>
      addStmt(Assert(a.expr, a.loc))

  def translateBlock(b: hil.Block, next: String, noFinalJump: Boolean = false): Unit =
    val size = b.stmts.length
    @tailrec
    def iter(stmts: List[hil.Stmt], i: Int): Unit =
      if stmts.nonEmpty then
        val isLast = i == size - 1
        val stmt = stmts.head
        if isControlStmt(stmt) then
          if !isLast then
            val newNext = mkLabel()
            translateStmt(stmt, newNext)
            startBlock(newNext)
            iter(stmts.tail, i + 1)
          else translateStmt(stmt, next)
        else if isBreakOrContinue(stmt) then
          translateStmt(stmt, next)
        else
          translateStmt(stmt, next)
          if isLast && !isBlockFinished && !noFinalJump then
            val j = Jump(next, b.loc)
            finishBlock(j)
          else iter(stmts.tail, i + 1)

    iter(b.stmts, 0)

  def isControlStmt(stmt: hil.Stmt): Boolean = stmt match
    case _: hil.IfThenElse => true
    case _: hil.Loop       => true
    case _: hil.WhileLoop  => true
    case _                 => false

  def isBreakOrContinue(stmt: hil.Stmt): Boolean = stmt match
    case _: hil.Break    => true
    case _: hil.Continue => true
    case _               => false

  def addStmt(stmt: Stmt): Unit =
    blockStmts.addOne(stmt)

  def startBlock(label: String): Unit =
    if blockStmts.nonEmpty then
      println(s"Start block $label while stmts buffer is not empty")
    currLabel = label
    isBlockFinished = false
    blockStmts.clear()

  def finishBlock(transfer: Transfer): Unit =
    if isBlockFinished then
      println(s"Trying to finish block $currLabel, but it was already finished")
    val stmts = blockStmts.toList
    blockStmts.clear
    val block = Block(currLabel, stmts, transfer, transfer.loc)
    blocks.addOne(block)
    isBlockFinished = true

  def mkLabel(): String =
    labelCount += 1
    s"bb$labelCount"

object HilToLil:
  def apply(hprog: hil.Program): Program =
    val funcs = hprog.funcs.map(f => new HilToLil().translateFunc(f))
    Program(funcs, hprog.loc)
