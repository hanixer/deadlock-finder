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
      val thenStmt1 = transformStmt(ite.thenStmt)
      val thenStmt2 = addStmtOrMakeBlock(thenStmt1, positiveAssert)
      val elseStmt = ite.elseStmt.map { s =>
        val s1 = transformStmt(s)
        addStmtOrMakeBlock(s1, negativeAssert)
      }.orElse(Some(negativeAssert))
      ite.copy(thenStmt = thenStmt2, elseStmt = elseStmt)
    case l: Loop =>
      val body = transformStmt(l.body)
      l.copy(body = body)
    case b: Block =>
      transformBlock(b)
    case _ => stmt

  private def transformBlock(b: Block): Block =
    val stmts = b.stmts.map(transformStmt)
    b.copy(stmts = stmts)

  private def addStmtOrMakeBlock(where: Stmt, what: Stmt): Stmt = where match
    case b: Block =>
      b.copy(stmts = what :: b.stmts)
    case _ =>
      val stmts = List(what, where)
      Block(stmts, where.loc)
