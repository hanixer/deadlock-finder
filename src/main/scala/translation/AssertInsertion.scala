package deadlockFinder
package translation

import hil.*
import common.UnaryOp

/** This pass adds assertion statements to IfThenElse branches.
 *  It will add an assertion even to empty else branches.
 */
object AssertInsertion:
  def apply(program: Program): Program =
    val funcs = program.funcs.map(transformFunc)
    program.copy(funcs = funcs)

  private def transformFunc(func: FuncDecl): FuncDecl =
    val body = transformBlock(func.body)
    func.copy(body = body)

  private def transformStmt(stmt: Stmt): Stmt = stmt match
    case ite: IfThenElse =>
      val positiveAssert = Assert(ite.cond, ite.cond.loc)
      val negation = UnaryExpr(UnaryOp.Not, ite.cond, ite.cond.loc)
      val negativeAssert = Assert(negation, ite.cond.loc)
      val thenStmt1 = transformBlock(ite.thenStmt)
      val thenStmt2 = addStmtOrMakeBlock(thenStmt1, positiveAssert)
      val elseStmt = ite.elseStmt.map { s =>
        val s1 = transformBlock(s)
        addStmtOrMakeBlock(s1, negativeAssert)
      }.orElse(Some(Block(List(negativeAssert), negativeAssert.loc)))
      
      ite.copy(thenStmt = thenStmt2, elseStmt = elseStmt)
      
    case l: Loop =>
      val body = transformBlock(l.body)
      l.copy(body = body)
      
    case b: Block =>
      transformBlock(b)
      
    case _ => stmt

  private def transformBlock(b: Block): Block =
    val stmts = b.stmts.map(transformStmt)
    b.copy(stmts = stmts)

  private def addStmtOrMakeBlock(where: Block, what: Stmt): Block =
    val stmts = what :: where.stmts
    where.copy(stmts = stmts)
