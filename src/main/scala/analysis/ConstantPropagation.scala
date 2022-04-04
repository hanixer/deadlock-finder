package deadlockFinder
package analysis

import expr.*
import lil.*
import common.*
import scala.annotation.tailrec

object ConstantPropagation:
  /** Constant abstract value. */
  enum ConstantAbsVal:
    case Undefined
    case Constant(n: Int)
    case NotConstant

  extension (const: ConstantAbsVal)
    def isLower(other: ConstantAbsVal): Boolean = const match
      case ConstantAbsVal.NotConstant => true
      case ConstantAbsVal.Constant(_) =>
        other match
          case ConstantAbsVal.Undefined => true
          case _                        => false
      case ConstantAbsVal.Undefined => false
    def isConstant: Boolean = const match
      case ConstantAbsVal.Constant(_) => true
      case _                          => false

  type ConstantsMap = Map[VarInfo, ConstantAbsVal]

  extension (consts: ConstantsMap)
    def lookup(v: AbstractVar): ConstantAbsVal = lookup(VarInfo(v))
    def lookup(v: VarInfo): ConstantAbsVal = 
      consts.getOrElse(v, ConstantAbsVal.Undefined)

  def evalBinopInt(op: BinaryOp, n1: Int, n2: Int): ConstantAbsVal = op match
    case BinaryOp.Plus   => ConstantAbsVal.Constant(n1 + n2)
    case BinaryOp.Minus  => ConstantAbsVal.Constant(n1 - n2)
    case BinaryOp.Times  => ConstantAbsVal.Constant(n1 * n2)
    case BinaryOp.Divide => ConstantAbsVal.Constant(n1 / n2)
    case _               => ConstantAbsVal.NotConstant

  def evalBinopConst(
      op: BinaryOp,
      lhs: ConstantAbsVal,
      rhs: ConstantAbsVal
  ): ConstantAbsVal =
    lhs match
      case ConstantAbsVal.NotConstant => ConstantAbsVal.NotConstant
      case ConstantAbsVal.Constant(n1) =>
        rhs match
          case ConstantAbsVal.NotConstant  => ConstantAbsVal.NotConstant
          case ConstantAbsVal.Constant(n2) => evalBinopInt(op, n1, n2)
          case ConstantAbsVal.Undefined    => ConstantAbsVal.Undefined
      case ConstantAbsVal.Undefined => ConstantAbsVal.Undefined

  def mergeValues(v1: ConstantAbsVal, v2: ConstantAbsVal): ConstantAbsVal =
    v1 match
      case ConstantAbsVal.NotConstant => ConstantAbsVal.NotConstant
      case ConstantAbsVal.Constant(n1) =>
        v2 match
          case ConstantAbsVal.Undefined                => v1
          case ConstantAbsVal.Constant(n2) if n1 == n2 => v1
          case _ => ConstantAbsVal.NotConstant
      case ConstantAbsVal.Undefined => v2

  /** Collect immediate constants and add to map. Other variables marked
    * undefined.
    */
  def getImmediateConstants(func: FuncDecl): ConstantsMap =
    def visitSimpleExpr(expr: SimpleExpr): ConstantAbsVal = expr match
      case i: IntLiteral => ConstantAbsVal.Constant(i.n)
      case _             => ConstantAbsVal.Undefined

    def visitExpr(expr: Expr): ConstantAbsVal = expr match
      case BinaryExpr(op, IntLiteral(n1, _), IntLiteral(n2, _), _) =>
        evalBinopInt(op, n1, n2)
      case se: SimpleExpr => visitSimpleExpr(se)
      case _              => ConstantAbsVal.Undefined

    def visitStmt(stmt: Stmt): Option[(VarInfo, ConstantAbsVal)] = stmt match
      case vd: VarDecl =>
        val vi = VarInfo(vd.v)
        vd.rhs
          .map(visitExpr)
          .map(cv => (vi, cv))
          .orElse(Some(vi, ConstantAbsVal.Undefined))
      case a: Assignment =>
        val vi = VarInfo(a.lhs)
        Some(vi, visitExpr(a.rhs))
      case _ => None

    def visitBlock(block: Block): List[(VarInfo, ConstantAbsVal)] =
      val paramsM =
        block.params.flatMap(p => Some(VarInfo(p.v), ConstantAbsVal.Undefined))
      val stmtsM = block.stmts.flatMap(visitStmt)
      paramsM ++ stmtsM

    func.body.flatMap(visitBlock).toMap

  case class BlockArg(label: String, pos: Int)

  /** Build a map that answers a question: Which variables are passed to a block
    * to a given position?
    */
  def buildBlockArgsMap(func: FuncDecl): Map[BlockArg, Set[VarInfo]] =
    def visitArgs(
        args: List[AbstractVar],
        label: String
    ): List[(BlockArg, VarInfo)] =
      args.zipWithIndex.map((v, pos) => (BlockArg(label, pos), VarInfo(v)))

    def visitBlock(block: Block): List[(BlockArg, VarInfo)] =
      block.transfer match
        case j: Jump => visitArgs(j.vars, j.label)
        case cj: CondJump =>
          visitArgs(cj.thenArgs, cj.thenLabel) ++ visitArgs(
            cj.elseArgs,
            cj.elseLabel
          )
        case _ => List.empty

    val pairs = func.body.flatMap(visitBlock)
    pairs.groupMap(_._1)(_._2).map((k, v) => (k, v.toSet))

  def computeConstants(func: FuncDecl): ConstantsMap =
    def evalExpr(expr: Expr, consts: ConstantsMap): ConstantAbsVal = expr match
      case b: BinaryExpr =>
        val lhs = evalExpr(b.lhs, consts)
        val rhs = evalExpr(b.rhs, consts)
        evalBinopConst(b.op, lhs, rhs)
      case v: AbstractVar => consts.lookup(v)
      case i: IntLiteral  => ConstantAbsVal.Constant(i.n)
      case _              => ConstantAbsVal.NotConstant

    def mergeVars(
        varInfos: Set[VarInfo],
        consts: ConstantsMap
    ): ConstantAbsVal =
      varInfos.foldLeft(ConstantAbsVal.Undefined)((val1, varInfo2) =>
        val val2 = consts.lookup(varInfo2)
        mergeValues(val1, val2)
      )

    val usesAndDefs = UsesAndDefs(func)
    val blockArgsMap = buildBlockArgsMap(func)

    def getDefAndNewValue(
        use: Use,
        consts: ConstantsMap
    ): (Def, ConstantAbsVal) =
      use match
        case vd: Use.VDecl =>
          val d = Def(vd.decl)
          val newV = vd.decl.rhs
            .map(e => evalExpr(e, consts))
            .getOrElse(ConstantAbsVal.Undefined)
          (d, newV)
        case a: Use.Assign =>
          val d = Def(a.assign)
          val newV = evalExpr(a.assign.rhs, consts)
          (d, newV)
        case bp: Use.BParam =>
          val varInfos = blockArgsMap(BlockArg(bp.label, bp.index))
          val newV = mergeVars(varInfos, consts)
          val block = func.labelToBlock(bp.label) 
          val varInfoDef = VarInfo(block.params(bp.index).v)
          val d = Def.BParam(varInfoDef, bp.label, bp.index)
          (d, newV)

    @tailrec
    def iter(todo: List[Use], consts: ConstantsMap): ConstantsMap = todo match
      case use :: rest =>
        // Determine def and new value.
        val (d, newV) = getDefAndNewValue(use, consts)
        val oldV = consts.lookup(d.varInfo)
        // Check if new value is lower than the old one.
        if newV != oldV && newV.isLower(oldV) then
          // Add new items to worklist, update constants map.
          val consts1 = consts.updated(d.varInfo, newV)
          val uses = usesAndDefs.getUsesForDef(d)
          val todo1 = rest ++ uses
          iter(todo1, consts1)
        else
          // Go to the next item
          iter(rest, consts)

      case _ => consts

    val immediate = getImmediateConstants(func)
    // Initialize worklist by finding uses of immediate constants.
    val todo = immediate
      .filter((_, v) => v.isConstant)
      .keys
      .flatMap(usesAndDefs.getUsesOfVar)
      .toList

    iter(todo, immediate)

  end computeConstants

  def computeConstantsSimplified(func: FuncDecl): Map[String, Int] =
    val constantsMap = computeConstants(func)
    // Entries gathered:
    // - name has only 1 index
    // - index = 0
    // - abstract value is constant
    constantsMap.toList.groupBy(_._1.name)
      .collect { case (name, (VarInfo(_, Some(0)), ConstantAbsVal.Constant(n)) :: Nil) =>
            (name, n)
      }

end ConstantPropagation

// 1. Run all expressions, evaluate immediate constant expression,
//    i.e. those containing only immediate constants.
// 2. All other variables are initialized to top (undefined).
// 3. Add to worklist all SSA edges from a constant definition to uses.
// 4. If worklist is empty - end algorithm.
// 5. Take item from worklist.
// 6. Take destination and evaluate it.
// 7. If resulting value is lower than current, replace it.
//    Also, add all SSA edges from current assignment.
// 8. Go to step 4.

// block -> var map -> var map
// stmt -> var map -> var map
