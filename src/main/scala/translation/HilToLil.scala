package deadlockFinder
package translation

import common.VoidType
import hil.{IfThenElse, Variable}
import lil.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

class HilToLil:
  val blockStmts: ListBuffer[Stmt] = ListBuffer()
  val blocks: ListBuffer[Block] = ListBuffer()
  var labelCount: Int = 0
  var currLabel: String = mkLabel()
  var loopStack: mutable.Stack[(String, String)] =
    mutable.Stack() // Stack of (loopStart, loopEnd) pairs

  def translateFunc(func: hil.FuncDecl): FuncDecl =
    translateStmt(func.body, "end")
    addEndBlock(func)
    val entry = Block("entry", List.empty, Jump(blocks.head.label, func.loc), func.loc)
    blocks.prepend(entry)
    FuncDecl(func.name, func.params, func.retTyp, blocks.toList, func.loc)

  def addEndBlock(func: hil.FuncDecl): Unit =
    startBlock("end")
    val loc = func.loc
    if func.retTyp != VoidType then
      finishBlock(Return(Some(Variable("~retVal", loc)), loc))
    else finishBlock(Return(None, loc))

  def translateStmt(stmt: hil.Stmt, next: String): Unit = stmt match
    case b: hil.Block =>
      translateBlock(b, next)

    case i: hil.IfThenElse =>
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

    case l: hil.Loop =>
      val isNewBlock = blockStmts.nonEmpty
      val startLabel = if isNewBlock then mkLabel() else currLabel
      loopStack.push((startLabel, next))

      if isNewBlock then
        finishBlock(Jump(startLabel, l.loc))
        startBlock(startLabel)

      translateStmt(l.body, startLabel)
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

  def translateBlock(b: hil.Block, next: String): Unit =
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

  // def translateExpr(hexpr: hil.Expr): Expr = hexpr match
  //   case i: IntLiteral           => IntLiteral(i.n, i.loc)
  //   case v: Variable             => Variable(v.name, v.loc)
  //   case b: BinaryExpr           => BinaryExpr(b.op, b.lhs, b.rhs, b.loc)
  //   case u: UnaryExpr            => UnaryExpr(u.op, u.e, u.loc)
  //   case c: CallExpr             => CallExpr(c.name, c.args, c.loc)
  //   case u: UnsupportedConstruct => UnsupportedConstruct(u.loc)

  def isControlStmt(stmt: hil.Stmt): Boolean = stmt match
    case _: hil.IfThenElse => true
    case _: hil.Loop       => true
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
    blockStmts.clear()

  def finishBlock(transfer: Transfer): Unit =
    val stmts = blockStmts.toList
    blockStmts.clear
    val block = Block(currLabel, stmts, transfer, transfer.loc)
    blocks.addOne(block)

  def mkLabel(): String =
    labelCount += 1
    s"bb$labelCount"

object HilToLil:
  def apply(hprog: hil.Program): Program =
    val funcs = hprog.funcs.map(f => new HilToLil().translateFunc(f))
    Program(funcs, hprog.loc)
