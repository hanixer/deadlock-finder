package deadlockFinder
package translation

import analysis.UsesAndDefs
import hil.*

class LoopUnrolling(func: FuncDecl, usesAndDefs: UsesAndDefs):
  def transform(): FuncDecl =

    ???

object LoopUnrolling:
  def apply(program: Program): Program =
    val ssaProgram = LilToSsa(HilToLil(program))
    val funcs = program.funcs.map { f =>
      val ssaFunc =
        ssaProgram.funcByName(f.name).getOrElse(throw new Exception(s"Function ${f.name} is not found in SSA program"))
      val usesAndDefs = UsesAndDefs(ssaFunc)
      transformFunc(f, usesAndDefs)
    }

    ???

  def transformFunc(func: FuncDecl, usesAndDefs: UsesAndDefs): FuncDecl =
    ???

  def transformStmt(stmt: Stmt): Stmt = stmt match
    case wl: WhileLoop => transformWhile(wl)
    case l: Loop =>
      val body = transformBlock(l.body)
      l.copy(body = body)
    case ite: IfThenElse =>
      val thenS = transformBlock(ite.thenBlock)
      val elseS = ite.elseBlock.map(transformBlock)
      ite.copy(thenBlock = thenS, elseBlock = elseS)
    case _ => stmt

  def transformWhile(loop: WhileLoop): WhileLoop =
    // Get condition expression
    // If it's x < const
    // then continue;
    // Find loop variable:
    // Take last stmt
    // If it is of form
    // x = x + const - take it
    // we constant, name of loop var, update amount
    // we also need initial expression of loop variable
    ???

  def transformBlock(block: Block): Block =
    val stmts = block.stmts.map(transformStmt)
    block.copy(stmts = stmts)

