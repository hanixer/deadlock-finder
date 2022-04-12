package deadlockFinder
package parteval

import expr.*
import lil.*
import cfg.CfgGraph
import common.NameCounter
import common.SourceLoc


import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum Value:
  case Intg(n: Int)
  case Bool(b: Boolean)

class Reduction(func: FuncDecl):
  /** Maps from a variable name to its current value. */
  type Store = Map[String, Value]
  case class LabelAndStore(label: String, store: Store)

  private val cfg = CfgGraph(func)
  private val division = new Division(func)
  private val stmts = ListBuffer.empty[Stmt]
  private var store: Store = Map.empty[String, Value]
  private val queue = mutable.Queue.empty[LabelAndStore]
  private val nameCounter = new NameCounter
  private val newLabels = mutable.Map.empty[LabelAndStore, String]

  def transform(): FuncDecl =
    queue.enqueue(LabelAndStore(cfg.entry, store))
    val blocks = ListBuffer.empty[Block]
    val end = "end"
    val seen = mutable.Set.empty[LabelAndStore]

    while queue.nonEmpty do
      val ls = queue.dequeue()
      if seen.add(ls) then
        val block = func.labelToBlock(ls.label)
        store = ls.store
        for s <- block.stmts do
          handleStmt(s)
        val transfer =
          if ls.label == end then
            // Jump to new end block.
            Jump(end, func.loc)
          else
            handleTransfer(block.transfer)
        val newLabel = getOrCreateLabel(ls)
        blocks.addOne(Block(newLabel, stmts.toList, transfer, block.loc))
        stmts.clear()

    val endBlock = Block(end, List.empty, Return(None, func.loc), func.loc)
    blocks.addOne(endBlock)

    func.copy(body = blocks.toList, entryLabel = blocks.head.label)

  private def handleStmt(s: Stmt) =
    s match
      case a: Assignment =>
        if division.isDynamic(a.lhs.name) then
          val rhs = reduce(a.rhs, store)
          val asg = a.copy(rhs = rhs)
          stmts.addOne(asg)
        else
          val value = evaluate(a.rhs, store)
          store = store.updated(a.lhs.name, value)
      case _ =>
        stmts.addOne(s)

  private def handleTransfer(transfer: Transfer) =
    transfer match
      case j: Jump =>
        val next = LabelAndStore(j.label, store)
        queue.enqueue(next)
        val label = getOrCreateLabel(next)
        j.copy(label = label )
      case c: CondJump =>
        val isCondDynamic =
          c.cond match
            case v: Variable if !division.isDynamic(v.name) => false
            case b: BoolLiteral => false
            case _ => true
        if isCondDynamic then
          val thenLS = LabelAndStore(c.thenLabel, store)
          val elseLS = LabelAndStore(c.elseLabel, store)
          val thenLabel = getOrCreateLabel(thenLS)
          val elseLabel = getOrCreateLabel(elseLS)
          queue.enqueue(thenLS)
          queue.enqueue(elseLS)
          c.copy(thenLabel = thenLabel, elseLabel = elseLabel)
        else
          val condVal = evaluate(c.cond, store)
          condVal match
            case Value.Bool(true) =>
              handleCondBranch(c.thenLabel, c.loc)
            case Value.Bool(false) =>
              handleCondBranch(c.elseLabel, c.loc)
            case x => throw new Exception(s"Expected bool in condition but got $x")
      case _ => transfer

  private def handleCondBranch(targetLabel: String, loc: SourceLoc) =
    val next = LabelAndStore(targetLabel, store)
    queue.enqueue(next)
    val label = getOrCreateLabel(next)
    Jump(label, loc)

  def reduce(expr: Expr, store: Store): Expr =
    expr match
      case b: BoolLiteral => b
      case i: IntLiteral => i
      case v: Variable =>
        if division.isDynamic(v.name) then
          v
        else
          evaluate(v, store) match
            case b: Value.Bool => BoolLiteral(b.b, expr.loc)
            case i: Value.Intg => IntLiteral(i.n, expr.loc)
      case u: UnaryExpr =>
        reduce(u.e, store) match
          case UnaryExpr(UnaryOp.Not, e, _) => e
      case b: BinaryExpr =>
        val lhs = reduce(b.lhs, store).asInstanceOf[SimpleExpr]
        val rhs = reduce(b.rhs, store).asInstanceOf[SimpleExpr]
        b.copy(lhs = lhs, rhs = rhs)
      case _ => expr

  def evaluate(expr: Expr, store: Store): Value =
    expr match
      case b: BoolLiteral => Value.Bool(b.b)
      case i: IntLiteral => Value.Intg(i.n)
      case v: Variable =>
        store.getOrElse(v.name, throw new Exception(s"Variable ${v.name} not found"))
      case u: UnaryExpr =>
        evaluate(u.e, store) match
          case Value.Bool(true) => Value.Bool(false)
          case Value.Bool(false) => Value.Bool(true)
          case v => throw new Exception(s"Bool expected, but got $v")
      case b: BinaryExpr =>
        if b.op.isArithmetic then
          evaluate(b.lhs, store) match
            case Value.Intg(lhs) =>
              evaluate(b.rhs, store) match
                case Value.Intg(rhs) =>
                  evaluateArithmetic(b.op, lhs, rhs)
                case v => throw new Exception(s"Int expected, but got $v")
            case v => throw new Exception(s"Int expected, but got $v")
        else if b.op.isRelation then
          evaluate(b.lhs, store) match
            case Value.Intg(lhs) =>
              evaluate(b.rhs, store) match
                case Value.Intg(rhs) =>
                  evaluateRelation(b.op, lhs, rhs)
                case v => throw new Exception(s"Int expected, but got $v")
            case v => throw new Exception(s"Int expected, but got $v")
        else
          throw new Exception(s"Wrong operation for evaluation: ${b.op}")
      case _ =>
        throw new Exception(s"Cannot evaluate $expr")

  def evaluateArithmetic(op: BinaryOp, lhs: Int, rhs: Int): Value =
    val n = op match
      case BinaryOp.Plus => lhs + rhs
      case BinaryOp.Minus => lhs - rhs
      case BinaryOp.Times => lhs * rhs
      case BinaryOp.Divide => lhs / rhs
      case _ => throw new Exception(s"Wrong operation $op, expected arithmetic op")
    Value.Intg(n)

  def evaluateRelation(op: BinaryOp, lhs: Int, rhs: Int): Value =
    val v = op match
      case BinaryOp.Less => lhs < rhs
      case BinaryOp.Greater => lhs > rhs
      case BinaryOp.LessEquals => lhs <= rhs
      case BinaryOp.GreaterEquals => lhs >= rhs
      case BinaryOp.Equals => lhs == rhs
      case _ => throw new Exception(s"Wrong operation $op, expected relation op")
    Value.Bool(v)

  def getOrCreateLabel(ls: LabelAndStore): String =
    newLabels.get(ls) match
      case Some(label) => label
      case None =>
        val label = nameCounter.newName(ls.label)
        newLabels.put(ls, label)
        label