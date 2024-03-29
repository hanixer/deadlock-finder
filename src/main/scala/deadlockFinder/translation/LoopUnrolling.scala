package deadlockFinder
package translation

import analysis.{ConstantPropagation, UsesAndDefs, VarInfo}
import expr.*
import hil.*

class LoopUnrolling(func: FuncDecl, usesAndDefs: UsesAndDefs, constants: Map[String, Int]):
  def transform(): FuncDecl =
    val body = transformBlock(func.body)
    func.copy(body = body)

  def transformStmt(stmt: Stmt): Stmt = stmt match
    case wl: WhileLoop =>
      unrollLoop(wl).map { stmts => Block(stmts, wl.loc) }
        .getOrElse(wl)
    case ite: IfThenElse =>
      val thenS = transformBlock(ite.thenBlock)
      val elseS = ite.elseBlock.map(transformBlock)
      ite.copy(thenBlock = thenS, elseBlock = elseS)
    case b: Block => transformBlock(b)
    case _ => stmt

  def transformBlock(block: Block): Block =
    val stmts = block.stmts.map(transformStmt)
    block.copy(stmts = stmts)

  def unrollLoop(loop: WhileLoop): Option[List[Stmt]] =
    // Get condition expression
    // If it's x < const
    // then continue;
    // Find loop variable:
    // Take last stmt
    // If it is of form
    // x = x + const - take it
    // we need constant, name of loop var, update amount
    // we also need initial expression of loop variable
    // How can we map between variable and SSA variable???

    // t~1 = i < 5
    // t~1 = i <= 5
    // t~1 = i > 5
    // t~1 = i >= 5
    // cond t~1

    loop.condition match
      case v: Variable =>
        usesAndDefs.getInitialDefiningExpr(v.name) match
          case Some(BinaryExpr(BinaryOp.Less, lv: AbstractVar, IntLiteral(m, _), _)) =>
            usesAndDefs.getInitialDefiningExpr(lv.name) match
              case Some(IntLiteral(n, _)) =>
                getChangeAmount(loop.body.stmts, lv).flatMap { change =>
                  unrollLoop(loop.body, n, m, change)
                }
              case _ => None
          case e =>
            None
      case _ => None


  def unrollLoop(body: Block, start: Int, endExclusive: Int, change: Int): Option[List[Stmt]] =
    val stmts = Seq.range(start, endExclusive, change).toList
      .flatMap(_ => body.stmts)
    Some(stmts)

  def getChangeAmount(stmts: List[Stmt], variable: AbstractVar): Option[Int] =
    stmts.lastOption match
      case Some(Assignment(name, BinaryExpr(op, lhs: AbstractVar, IntLiteral(n, _), _), _))
        if name == variable.name && lhs.name == variable.name =>
          op match
            case BinaryOp.Plus => Some(n)
            case BinaryOp.Minus => Some(-n)
            case _ => None
      case _ => None

object LoopUnrolling:
  def apply(program: Program): Program =
    val ssaProgram = LilToSsa(HilToLil(program))
    val funcs = program.funcs.map { hilFunc =>
      val ssaFunc = ssaProgram.funcByName(hilFunc.name)
        .getOrElse(throw new Exception(s"Function ${hilFunc.name} is not found in SSA program"))
      val usesAndDefs = UsesAndDefs(ssaFunc)
      val constants = ConstantPropagation.computeConstantsSimplified(ssaFunc)
      new LoopUnrolling(hilFunc, usesAndDefs, constants).transform()
    }
    program.copy(funcs = funcs)
