package deadlockFinder
package translation

import lil.*

import scala.collection.mutable.ListBuffer
import deadlockFinder.hir.IfThenElse

import scala.collection.mutable.Stack
import deadlockFinder.common.VoidType

import scala.annotation.tailrec

class HirToLil:
  val blockStmts: ListBuffer[Stmt] = ListBuffer()
  val blocks: ListBuffer[Block] = ListBuffer()
  var labelCount: Int = 0
  var currLabel: String = mkLabel()
  var loopStack: Stack[(String, String)] =
    Stack() // Stack of (loopStart, loopEnd) pairs

  def translateFunc(func: hir.FuncDecl): FuncDecl =
    translateStmt(func.body, "end")
    addEndBlock(func)
    FuncDecl(func.name, func.params, func.retTyp, blocks.toList, func.loc)

  def addEndBlock(func: hir.FuncDecl): Unit =
    startBlock("end")
    val loc = func.loc
    if func.retTyp != VoidType then
      finishBlock(Return(Some(hir.Variable("~retVal", loc)), loc))
    else finishBlock(Return(None, loc))

  def translateStmt(stmt: hir.Stmt, next: String): Unit = stmt match
    case b: hir.Block =>
      translateBlock(b, next)

    case i: hir.IfThenElse =>
      val thenLabel = mkLabel()
      val elseLabel = if i.elseStmt.isDefined then mkLabel() else next
      finishBlock(CondJump(i.cond, thenLabel, elseLabel, i.loc))
      // Then branch
      startBlock(thenLabel)
      translateStmt(i.thenStmt, next)

      // Else branch, if present
      if i.elseStmt.isDefined then
        startBlock(elseLabel)
        translateStmt(i.elseStmt.get, next)

    case l: hir.Loop =>
      val isNewBlock = blockStmts.nonEmpty
      val startLabel = if isNewBlock then mkLabel() else currLabel
      loopStack.push((startLabel, next))

      if isNewBlock then
        finishBlock(Jump(startLabel, l.loc))
        startBlock(startLabel)

      translateStmt(l.body, startLabel)
      loopStack.pop()

    case v: hir.VarDecl =>
      addStmt(VarDecl(v.name, v.t, v.rhs, v.loc))

    case a: hir.Assignment =>
      addStmt(Assignment(a.lhs, a.rhs, a.loc))

    case b: hir.Break =>
      if loopStack.isEmpty then
        throw new Error(s"break is used without loop at ${b.loc}")
      val label = loopStack.top._2
      finishBlock(Jump(label, b.loc))

    case c: hir.Continue =>
      if loopStack.isEmpty then
        throw new Error(s"continue is used without loop at ${c.loc}")
      val label = loopStack.top._1
      finishBlock(Jump(label, c.loc))

    case _ =>
      ???

  def translateBlock(b: hir.Block, next: String): Unit =
    val size = b.stmts.length
    @tailrec
    def iter(stmts: List[hir.Stmt], i: Int): Unit =
      if stmts.nonEmpty then
        val isLast = i == size - 1
        val stmt = stmts.head
        if isControlStmt(stmt) then
          if !isLast then
            val newNext = mkLabel()
            translateStmt(stmt, newNext)
            currLabel = newNext
            iter(stmts.tail, i + 1)
          else translateStmt(stmt, next)
        else if isBreakOrContinue(stmt) then translateStmt(stmt, next)
        else
          translateStmt(stmt, next)
          if isLast then
            val j = Jump(next, b.loc)
            finishBlock(j)
          else iter(stmts.tail, i + 1)

    iter(b.stmts, 0)

  // def translateExpr(hexpr: hir.Expr): Expr = hexpr match
  //   case i: IntLiteral           => IntLiteral(i.n, i.loc)
  //   case v: Variable             => Variable(v.name, v.loc)
  //   case b: BinaryExpr           => BinaryExpr(b.op, b.lhs, b.rhs, b.loc)
  //   case u: UnaryExpr            => UnaryExpr(u.op, u.e, u.loc)
  //   case c: CallExpr             => CallExpr(c.name, c.args, c.loc)
  //   case u: UnsupportedConstruct => UnsupportedConstruct(u.loc)

  def isControlStmt(stmt: hir.Stmt): Boolean = stmt match
    case i: hir.IfThenElse => true
    case l: hir.Loop       => true
    case _                 => false

  def isBreakOrContinue(stmt: hir.Stmt): Boolean = stmt match
    case i: hir.Break    => true
    case l: hir.Continue => true
    case _               => false

  def addStmt(stmt: Stmt): Unit =
    blockStmts.addOne(stmt)

  def startBlock(label: String): Unit =
    if blockStmts.nonEmpty then
      println(s"Start block $label while stmts buffer is not empty")
    currLabel = label
    blockStmts.clear()

  def finishBlock(transfer: Transfer): Unit =
    val stmts = blockStmts.toList
    blockStmts.clear
    val block = Block(currLabel, stmts, transfer, transfer.loc)
    blocks.addOne(block)

  def mkLabel(): String =
    labelCount += 1
    s"bb$labelCount"

object HirToLil:
  def apply(hprog: hir.Program): Program =
    val funcs = hprog.funcs.map(f => new HirToLil().translateFunc(f))
    Program(funcs, hprog.loc)
