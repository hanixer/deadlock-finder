package deadlockFinder
package parteval

import common.{BooleanType, IntType, NameCounter}
import expr.*
import hil.*

import scala.collection.mutable

class PredicateExpansion(func: FuncDecl, nameCounter: NameCounter, processCount: Int):
  case class Predicate(rank: Int, isEqual: Boolean):
    def negate: Predicate = copy(isEqual = !isEqual)

  type Env = List[Predicate]

  private val definingExpr = mutable.Map.empty[String, Expr]

  def transform(): FuncDecl =
    val body = transformBlock(func.body, Nil)
    func.copy(body = body)

  def transformStmt(stmt: Stmt, env: Env): Stmt = stmt match
    case ite: IfThenElse =>
      // Consider the condition.
      evaluatePredicate(ite.cond) match
        // If the condition is rank predicate, then
        case Some(predicate) =>
          //   add assert statement to the beginning of then block.
          val condAssert = Assert(ite.cond, ite.loc)
          val stmts = ite.thenBlock.stmts.map(transformStmt(_, env))
          val thenBlock = Block(condAssert :: stmts, ite.loc)
          val env2 = predicate.negate :: env
          val elseBlock =
            if shouldExpandElseBlock(ite.elseBlock) then expandElseBlock(ite.elseBlock, env2)
            else ite.elseBlock.map(transformBlock(_, env2))
          ite.copy(thenBlock = thenBlock, elseBlock = elseBlock)
        // Otherwise, recurse to children and transform them accordingly.
        case None =>
          val thenBlock = transformBlock(ite.thenBlock, env)
          val elseBlock = ite.elseBlock.map(transformBlock(_, env))
          ite.copy(thenBlock = thenBlock, elseBlock = elseBlock)
    case wl: WhileLoop =>
      val body = transformBlock(wl.body, env)
      wl.copy(body = body)
    case b: Block => transformBlock(b, env)
    case a: Assignment =>
      definingExpr.put(a.lhs, a.rhs)
      stmt
    case VarDecl(lhs, _, Some(rhs), _) =>
      definingExpr.put(lhs, rhs)
      stmt
    case _ => stmt

  def shouldExpandElseBlock(elseBlock: Option[Block]): Boolean = elseBlock match
    case Some(Block(List(_: VarDecl, ite2: IfThenElse), _)) =>
      evaluatePredicate(ite2.cond).isEmpty
    case None => false
    case _    => true

  /** Replicates else block for each of the determined ranks */
  def expandElseBlock(elseBlock: Option[Block], env: Env): Option[Block] =
    // TODO: do only one Rank() call and save result to a shared variable.
    elseBlock.map { block =>
      val ranks = ranksForExpansion(env)
      ranks.foldLeft(block) { (prevBlock, rank) =>
        val rankCall = CallExpr("mpi.Comm.Rank", Nil, block.loc)
        val callTemp = Variable(nameCounter.newName(), block.loc)
        val callTempDecl = VarDecl(callTemp.name, IntType, Some(rankCall), block.loc)
        val condTemp = Variable(nameCounter.newName(), block.loc)
        val literal = IntLiteral(rank, block.loc)
        val comparison = BinaryExpr(BinaryOp.Equals, callTemp, literal, block.loc)
        val condDecl = VarDecl(condTemp.name, BooleanType, Some(comparison), block.loc)
        val rankAssert = Assert(condTemp.copy(), block.loc)
        val thenStmts = rankAssert :: block.stmts
        val thenBlock = Block(thenStmts, block.loc)
        val ite = IfThenElse(condTemp, thenBlock, Some(prevBlock), block.loc)
        val stmts = List(callTempDecl, condDecl, ite)
        Block(stmts, block.loc)
      }
    }

  /** Determine which ranks should be included in the expansion */
  def ranksForExpansion(env: Env): Seq[Int] =
    for
      r <- 0 until processCount
      if !env.exists(_.rank == r)
    yield r

  def transformBlock(block: Block, env: Env): Block =
    val stmts = block.stmts.map(transformStmt(_, env))
    block.copy(stmts = stmts)

  def evaluatePredicate(expr: Expr): Option[Predicate] = expr match
    case v: Variable =>
      definingExpr.get(v.name) match
        case Some(expr) => evaluatePredicate(expr)
        case None       => None
    case UnaryExpr(UnaryOp.Not, e, _) =>
      evaluatePredicate(e) match
        case Some(predicate) => Some(predicate.copy(isEqual = !predicate.isEqual))
        case None            => None
    case BinaryExpr(op, lhs, n: IntLiteral, _) =>
      if isRankCall(lhs) then
        op match
          case BinaryOp.Equals => Some(Predicate(n.n, true))
          case _               => None
      else None
    case _ => None

  /** Returns true if a condTemp contains value of Rank() call or it is a Rank() call expression.
    */
  def isRankCall(expr: Expr): Boolean = expr match
    case v: Variable    => definingExpr.get(v.name).exists(isRankCall)
    case call: CallExpr => call.name == "mpi.Comm.Rank"
    case _              => false

object PredicateExpansion:
  def apply(prog: Program, processCount: Int = 5): Program =
    val nameCounter = new NameCounter(prog.tempCounter)
    val funcs = prog.funcs.map(new PredicateExpansion(_, nameCounter, processCount).transform())
    prog.copy(funcs = funcs, tempCounter = nameCounter.counter)
