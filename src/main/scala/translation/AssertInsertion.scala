package deadlockFinder
package translation

import expr.*
import hil.*

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
      val thenBlock1 = transformBlock(ite.thenBlock)
      val thenBlock2 = addStmtOrMakeBlock(thenBlock1, positiveAssert)
      val elseBlock = ite.elseBlock.map(transformBlock)
      ite.copy(thenBlock = thenBlock2, elseBlock = elseBlock)
      
    case wl: WhileLoop =>
      val condBlock = transformBlock(wl.condBlock)
      val body = transformBlock(wl.body)
      wl.copy(condBlock = condBlock, body = body)
      
    case b: Block =>
      transformBlock(b)
      
    case _ => stmt

  private def transformBlock(b: Block): Block =
    val stmts = b.stmts.map(transformStmt)
    b.copy(stmts = stmts)

  private def addStmtOrMakeBlock(where: Block, what: Stmt): Block =
    val stmts = what :: where.stmts
    where.copy(stmts = stmts)
