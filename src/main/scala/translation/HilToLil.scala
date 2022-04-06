package deadlockFinder
package translation

import common.{BooleanType, VoidType}
import expr.*
import lil.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

class HilToLil:
  private val blockStmts = ListBuffer.empty[Stmt]
  private val blocks = ListBuffer.empty[Block]
  private var labelCount: Int = 0
  private var currLabel: String = mkLabel()
  private var isBlockFinished: Boolean = false
  private val loopStack = mutable.Stack.empty[(String, String)] // Stack of (loopStart, loopEnd) pairs

  def translateFunc(func: hil.FuncDecl): FuncDecl =
    startBlock("entry")
    translateStmt(func.body)
    val jumpToEnd = Jump("end", func.loc)
    finishBlock(jumpToEnd)
    addEndBlock(func)
    FuncDecl(func.name, func.params, func.retTyp, blocks.toList, "entry", "end", func.loc)

  def addEndBlock(func: hil.FuncDecl): Unit =
    startBlock("end")
    val loc = func.loc
    if func.retTyp != VoidType then
      finishBlock(Return(Some(Variable("~retVal", loc)), loc))
    else finishBlock(Return(None, loc))

  def translateStmt(stmt: hil.Stmt): Unit = stmt match
    case b: hil.Block =>
      for stmt <- b.stmts do
        translateStmt(stmt)

    case i: hil.IfThenElse =>
      val thenLabel = mkLabel()
      val afterLabel = mkLabel()
      val elseLabel = if i.elseBlock.isDefined then mkLabel() else afterLabel
      finishBlock(CondJump(i.cond, thenLabel, elseLabel, i.loc))

      // Then branch
      startBlock(thenLabel)
      translateStmt(i.thenBlock)
      val jumpAfter = Jump(afterLabel, i.thenBlock.loc)
      finishBlock(jumpAfter)

      // Else branch, if present
      if i.elseBlock.isDefined then
        startBlock(elseLabel)
        translateStmt(i.elseBlock.get)
        finishBlock(jumpAfter.copy(loc = i.elseBlock.get.loc))

      startBlock(afterLabel)

    case wl: hil.WhileLoop =>
      val isNewBlock = blockStmts.nonEmpty
      val loopStartLabel = if isNewBlock then mkLabel() else currLabel
      val bodyLabel = mkLabel()
      val afterLabel = mkLabel()
      loopStack.push((loopStartLabel, afterLabel))

      // Start new block if needed
      if isNewBlock then
        finishBlock(Jump(loopStartLabel, wl.loc))
        startBlock(loopStartLabel)

      // Condition
      translateStmt(wl.condBlock)
      val condLoc = wl.condition.loc
      val condJump = CondJump(wl.condition, bodyLabel, afterLabel, condLoc)
      finishBlock(condJump)

      // Body
      startBlock(bodyLabel)
      translateStmt(wl.body)
      val jumpBack = Jump(loopStartLabel, wl.loc)
      finishBlock(jumpBack)
      loopStack.pop()

      startBlock(afterLabel)

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

    case r: hil.Return =>
      if r.expr.isDefined then
        addStmt(Assignment(Variable("~retVal", r.loc), r.expr.get, r.loc))
      finishBlock(Jump("end", r.loc))

  def isControlStmt(stmt: hil.Stmt): Boolean = stmt match
    case _: hil.IfThenElse => true
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
    if !isBlockFinished then
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
