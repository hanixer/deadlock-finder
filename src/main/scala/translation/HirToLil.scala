package deadlockFinder
package translation

import lil.*
import scala.collection.mutable.ListBuffer
import deadlockFinder.hir.IfThenElse
import scala.collection.mutable.Stack
import deadlockFinder.common.VoidType

class HirToLil:
  val blockStmts: ListBuffer[Stmt] = ListBuffer()
  val blocks: ListBuffer[Block] = ListBuffer()
  var currLabel = mkLabel()
  var loopStack: Stack[(String, String)] =
    Stack() // Stack of (loopStart, loopEnd) pairs
  var labelCount: Int = 0

  def translateFunc(func: hir.FuncDecl): FuncDecl =
    translateStmt(func.body, "end")
    addEndBlock(func)
    FuncDecl(func.name, func.params, func.retTyp, blocks.toList, func.loc)    

  def addEndBlock(func: hir.FuncDecl): Unit =
    currLabel = "end"
    blockStmts.clear
    val loc = func.loc
    if func.retTyp != VoidType then
      finishBlock(Return(Some(hir.Variable("~retVal", loc)), loc))
    else
      finishBlock(Return(None, loc))

  def translateStmt(stmt: hir.Stmt, next: String): Unit = stmt match
    case b: hir.Block =>
      translateBlock(b, next)

    case i: hir.IfThenElse =>
      val thenLabel = mkLabel()
      val elseLabel = if i.elseStmt.isDefined then mkLabel() else next
      finishBlock(CondJump(i.cond, thenLabel, elseLabel, i.loc))
      // Then branch
      currLabel = thenLabel
      blockStmts.clear
      translateStmt(i.thenStmt, next)
      
      // Else branch, if present
      if i.elseStmt.isDefined then
        currLabel = elseLabel
        blockStmts.clear
        translateStmt(i.elseStmt.get, next)

    case v: hir.VarDecl =>
      addStmt(VarDecl(v.name, v.t, v.rhs, v.loc))

    case a: hir.Assignment =>
      addStmt(Assignment(a.lhs, a.rhs, a.loc))

    case _ =>
      ???

  def translateBlock(b: hir.Block, next: String): Unit =
    val size = b.stmts.length
    def iter(stmts: List[hir.Stmt], i: Int): Unit =
      if !stmts.isEmpty then
        val isLast = i == size - 1
        val stmt = stmts.head
        if isControlStmt(stmt) then
          if !isLast then
            val newNext = mkLabel()
            translateStmt(stmt, newNext)
            currLabel = newNext
            iter(stmts.tail, i + 1)
          else translateStmt(stmt, next)
        else
          translateStmt(stmt, next)
          if isLast then
            val j = Jump(next, b.loc)
            finishBlock(j)
          else
            iter(stmts.tail, i + 1)

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

  def addStmt(stmt: Stmt): Unit =
    blockStmts.addOne(stmt)

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
