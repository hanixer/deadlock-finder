package deadlockFinder
package translation

import common.*
import hil.*

import org.eclipse.jdt.core.dom.{ArrayCreation as JdtArrayCreation, ArrayType as JdtArrayType, Assignment as JdtAssignment, Block as JdtBlock, Type as JdtType, *}

import javax.swing.tree.TreePath
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}
import scala.jdk.CollectionConverters.*

/** Translates java source code in the form of Eclipse JDT AST to HIL
  * (High-level intermediate language)
  */
object SourceToHil:
  def apply(cu: CompilationUnit): Program =
    val visitor = new Visitor(cu)
    visitor.start()
    Program(visitor.getFuncs)

class Visitor(compilationUnit: CompilationUnit) extends ASTVisitor:
  private val TranslateProperty = "Translate"
  private val funcs = ListBuffer[FuncDecl]()
  private val stmtsStack: mutable.Stack[ListBuffer[(Stmt, SourceLoc)]] =
    mutable.Stack()
  private var tempCounter = 0

  def start(): Unit = compilationUnit.accept(this)

  def getFuncs: List[FuncDecl] = funcs.toList

  def mkFullName(typ: TypeDeclaration): String =
    val b = typ.resolveBinding()
    if b != null then mkFullName(b)
    else typ.getName.getIdentifier

  def mkFullName(b: ITypeBinding): String =
    b.getQualifiedName

  def mkFullName(meth: MethodDeclaration): String =
    val n = meth.getName.getIdentifier
    mkFullName(meth.resolveBinding, n)

  def mkFullName(b: IMethodBinding, n: String): String =
    if b != null then
      val tb = b.getDeclaringClass
      if tb != null then s"${mkFullName(b.getDeclaringClass)}.$n"
      else n
    else n

  def mkSourceLoc(node: ASTNode): SourceLoc =
    val pos = node.getStartPosition
    SourceLoc(
      compilationUnit.getLineNumber(pos),
      compilationUnit.getColumnNumber(pos) + 1
    )

  def shouldMakeTemporary(node: Expression): Boolean =
    val p = node.getParent
    if p.isInstanceOf[Expression] && p.getNodeType != ASTNode.ASSIGNMENT then
      true
    else if p.getNodeType == ASTNode.IF_STATEMENT then true
    else if p.getNodeType == ASTNode.RETURN_STATEMENT then true
    else false

  def translateType(typeNode: JdtType): Type =
    val tb = typeNode.resolveBinding()
    translateType(tb)

  def translateType(binding: ITypeBinding): Type =
    if binding != null then
      if binding.isArray then
        val elementType = translateType(binding.getElementType)
        ArrayType(elementType)
      else if binding.isPrimitive then
        binding.getName match
          case "int"     => IntType
          case "double"  => DoubleType
          case "float"   => FloatType
          case "boolean" => BooleanType
          case _         => VoidType
      else ClassType()
    else
      println(s"Unresolved binding")
      VoidType

  def translateOperator(op: InfixExpression.Operator): BinaryOp = op match
    case InfixExpression.Operator.PLUS           => BinaryOp.Plus
    case InfixExpression.Operator.MINUS          => BinaryOp.Minus
    case InfixExpression.Operator.TIMES          => BinaryOp.Times
    case InfixExpression.Operator.DIVIDE         => BinaryOp.Divide
    case InfixExpression.Operator.LESS           => BinaryOp.Less
    case InfixExpression.Operator.GREATER        => BinaryOp.Greater
    case InfixExpression.Operator.LESS_EQUALS    => BinaryOp.LessEquals
    case InfixExpression.Operator.GREATER_EQUALS => BinaryOp.GreaterEquals
    case InfixExpression.Operator.EQUALS         => BinaryOp.Equals
    case InfixExpression.Operator.AND            => BinaryOp.And
    case InfixExpression.Operator.OR             => BinaryOp.Or
    case _ =>
      println("Unknown operator: $op")
      BinaryOp.Plus

  def translateBody(node: ASTNode): List[Stmt] =
    pushStmtStack()
    node.accept(this)
    popStmtStack().map(_._1)

  def translateAndWrapBody(node: ASTNode): Block =
    val stmts = translateBody(node)
    Block(stmts, mkSourceLoc(node))

  def translateVariable(node: ASTNode, binding: IVariableBinding): Unit =
    val name = binding.getName
    val loc = mkSourceLoc(node)
    val static = Modifier.isStatic(binding.getModifiers)
    if !binding.isField then
      val v = Variable(name, loc)
      saveResult(node, v)
    else if static then
      val className = mkFullName(binding.getDeclaringClass)
      val sfa = StaticFieldAccess(className, name, loc)
      saveResult(node, sfa)

  def translateVariable(node: Name): Unit =
    val binding = node.resolveBinding()
    if binding != null && binding.getKind == IBinding.VARIABLE then
      translateVariable(node, binding.asInstanceOf[IVariableBinding])

  def addTempVar(typ: Type, loc: SourceLoc, rhs: Option[Expr] = None): String =
    tempCounter += 1
    val name = s"t~$tempCounter"
    addStmt(VarDecl(name, typ, rhs, loc))
    name

  def pushStmtStack(): Unit =
    stmtsStack.push(ListBuffer())

  def popStmtStack(): List[(Stmt, SourceLoc)] =
    stmtsStack.pop().toList

  def popAndGetStmts(): List[Stmt] =
    popStmtStack().map(_._1)

  def addStmt(stmt: Stmt): Unit =
    stmtsStack.top.addOne(stmt, stmt.loc)

  def saveResult(node: ASTNode, result: Any): Unit =
    node.setProperty(TranslateProperty, result)

  def getResultExpr(node: ASTNode): Expr =
    Option(getResult[Expr](node))
      .getOrElse(UnsupportedConstruct(mkSourceLoc(node)))

  def getResultSimpleExpr(node: ASTNode): SimpleExpr =
    Option(getResult[SimpleExpr](node))
      .getOrElse(UnsupportedConstruct(mkSourceLoc(node)))

  def getResultStmt(node: ASTNode): Stmt =
    Option(getResult[Stmt](node))
      .getOrElse(UnsupportedConstruct(mkSourceLoc(node)))

  def getResult[A](node: ASTNode): A =
    node.getProperty(TranslateProperty).asInstanceOf[A]

  override def visit(node: CompilationUnit): Boolean =
    true

  override def endVisit(node: TypeDeclaration): Unit =
    // val funcs: ListBuffer[FuncDecl] = ListBuffer()
    val methods = node.getMethods
    for m <- methods do
      val prop = m.getProperty(TranslateProperty)
      if prop != null then
        val func = prop.asInstanceOf[FuncDecl]
        funcs += func
    saveResult(node, funcs.toList)

  override def visit(node: MethodDeclaration): Boolean =
    def translateParam(any: AnyRef): Param = {
      val varDecl = any.asInstanceOf[SingleVariableDeclaration]
      val name = varDecl.getName.getIdentifier
      val typ = translateType(varDecl.getType)
      val loc = mkSourceLoc(varDecl)
      Param(name, typ, loc)
    }

    // Gather parameters
    val params = node.parameters().asScala.toList.map(translateParam)

    // Gather return type
    val retType = translateType(node.getReturnType2)

    // Translate body
    val body = translateAndWrapBody(node.getBody)
    val loc = mkSourceLoc(node)
    val func = FuncDecl(mkFullName(node), params, retType, body, loc)

    saveResult(node, func)

    false

  override def visit(node: IfStatement): Boolean =
    // Condition
    node.getExpression.accept(this)
    val cond = getResultSimpleExpr(node.getExpression)

    // Then statement
    val thenStmt = translateAndWrapBody(node.getThenStatement)

    // Else statement
    val elseStmt = Option(node.getElseStatement).map(translateAndWrapBody)
    val loc = mkSourceLoc(node)

    addStmt(IfThenElse(cond, thenStmt, elseStmt, loc))

    false

  override def visit(node: ForStatement): Boolean =
    // Initializer
    val initializers = node.initializers().asScala.toList.collect { case vde: VariableDeclarationExpression =>
      vde.accept(this)
      val typ = translateType(vde.resolveTypeBinding())
      val frags = vde.fragments().asScala.toList
      translateDeclFragments(typ, frags)
    }.flatten

    // Condition
    pushStmtStack()
    val expression = node.getExpression
    expression.accept(this)
    val cond = getResultExpr(expression)
    val condLoc = mkSourceLoc(expression)
    val condStmts = popAndGetStmts()
    val condBlock = Block(condStmts, condLoc)

    // Body
    pushStmtStack()
    val body = node.getBody
    val bodyLoc = mkSourceLoc(node.getBody)
    val bodyStmts = translateBody(body)
    bodyStmts.foreach(addStmt) // Add all statements to the current level
    val stmts = popAndGetStmts()

    // Update
    pushStmtStack()
    val updateExpressions = node.updaters().asScala.toList.collect { case e: Expression =>
      e.accept(this)
      e
    }
    val updateStmts = popAndGetStmts()

    // Make body block, loop and wrapper block, containing initializers
    val bodyBlock = Block(stmts ++ updateStmts, bodyLoc)
    val loc = mkSourceLoc(node)
    val loop = WhileLoop(condBlock, cond, bodyBlock, loc)
    val wrapBlock = Block(initializers :+ loop, loc)

    addStmt(wrapBlock)

    false

  override def visit(node: WhileStatement): Boolean =
    val expression = node.getExpression
    val body = node.getBody

    // Condition
    pushStmtStack()
    expression.accept(this)
    val cond = getResultExpr(expression)
    val condLoc = mkSourceLoc(expression)
    val condStmts = popAndGetStmts()

    val condBlock = Block(condStmts, condLoc)
    // Body
    pushStmtStack()
    val bodyStmts = translateBody(body)
    bodyStmts.foreach(addStmt) // Add all statements to the current level
    val stmts = popAndGetStmts()
    val bodyLoc = mkSourceLoc(node.getBody)
    val bodyBlock = Block(stmts, bodyLoc)
    val loc = mkSourceLoc(node)

    val loop = WhileLoop(condBlock, cond, bodyBlock, loc)

    addStmt(loop)

    false

  def translateDeclFragments(typ: Type, fragments: List[Any]): List[VarDecl] =
    fragments.collect { case frag: VariableDeclarationFragment =>
      val name = frag.getName.getIdentifier
      val rhs = Option(frag.getInitializer).map(getResultExpr)
      val loc = mkSourceLoc(frag)
      VarDecl(name, typ, rhs, loc)
    }

  override def endVisit(node: VariableDeclarationStatement): Unit =
    val typ = translateType(node.getType)

    val varDecls = translateDeclFragments(typ, node.fragments.asScala.toList)

    varDecls.foreach(addStmt)

  override def endVisit(node: ReturnStatement): Unit =
    val expr = Option(node.getExpression).map(getResultSimpleExpr)
    val loc = mkSourceLoc(node)
    addStmt(Return(expr, loc))

  override def endVisit(node: ContinueStatement): Unit =
    val loc = mkSourceLoc(node)
    addStmt(Continue(loc))

  override def endVisit(node: BreakStatement): Unit =
    val loc = mkSourceLoc(node)
    addStmt(Break(loc))

  override def endVisit(node: ExpressionStatement): Unit =
    node.getExpression match
      case mi: MethodInvocation =>
        val e = mi.getProperty(TranslateProperty).asInstanceOf[CallExpr]
        val loc = mkSourceLoc(node)
        addStmt(CallStmt(e))
      case _ =>

  override def endVisit(node: MethodInvocation): Unit =
    val binding = node.resolveMethodBinding
    if binding != null then
      val argNodes =
        if Modifier.isStatic(binding.getModifiers) then
          node.arguments.asScala.toList
        else node.getExpression :: node.arguments.asScala.toList

      val args =
        argNodes.map(a => getResultSimpleExpr(a.asInstanceOf[Expression]))
      val name = mkFullName(binding, node.getName.getIdentifier)
      val loc = mkSourceLoc(node)
      val expr = CallExpr(name, args, loc)

      // Make temporary variable if the parent is an expression
      if shouldMakeTemporary(node) then
        val typ = translateType(node.resolveTypeBinding)
        val name = addTempVar(typ, loc, Some(expr))
        saveResult(node, Variable(name, loc))
      else saveResult(node, expr)
    else println(s"Unresolved method: $node")

  override def endVisit(node: PrefixExpression): Unit =
    val loc = mkSourceLoc(node)
    val typ = translateType(node.resolveTypeBinding)

    def translateIncrement(op: BinaryOp): SimpleExpr =
      val operand = getResultSimpleExpr(node.getOperand).asInstanceOf[Variable]
      val rhs = BinaryExpr(op, operand, IntLiteral(1, loc), loc)
      val asgn = Assignment(operand.name, rhs, loc)
      addStmt(asgn)
      operand

    val expr =
      if node.getOperator == PrefixExpression.Operator.INCREMENT then
        translateIncrement(BinaryOp.Plus)
      else if node.getOperator == PrefixExpression.Operator.DECREMENT then
        translateIncrement(BinaryOp.Minus)
      else if node.getOperator == PrefixExpression.Operator.NOT then
        val op = UnaryOp.Not
        val e = getResultSimpleExpr(node.getOperand)
        val expr = UnaryExpr(op, e, loc)
        val name = addTempVar(typ, loc, Some(expr))
        Variable(name, loc)
      else
        val op =
          if node.getOperator == PrefixExpression.Operator.MINUS then
            BinaryOp.Minus
          else BinaryOp.Plus
        val lhs = IntLiteral(0, loc)
        val rhs = getResultSimpleExpr(node.getOperand)
        val expr = BinaryExpr(op, lhs, rhs, loc)
        val name = addTempVar(typ, loc, Some(expr))
        Variable(name, loc)

    saveResult(node, expr)

  override def endVisit(node: PostfixExpression): Unit =
    val loc = mkSourceLoc(node)
    val typ = translateType(node.resolveTypeBinding)

    def translate(op: BinaryOp): Unit =
      val operand = getResultSimpleExpr(node.getOperand).asInstanceOf[Variable]
      val temp = addTempVar(typ, loc, Some(operand))
      val rhs = BinaryExpr(op, operand, IntLiteral(1, loc), loc)
      val asgn = Assignment(operand.name, rhs, loc)
      addStmt(asgn)
      val expr = Variable(temp, loc)
      saveResult(node, expr)

    if node.getOperator == PostfixExpression.Operator.INCREMENT then
      translate(BinaryOp.Plus)
    else if node.getOperator == PostfixExpression.Operator.DECREMENT then
      translate(BinaryOp.Minus)


  override def endVisit(node: InfixExpression): Unit =
    val tb = node.resolveTypeBinding

    val op = translateOperator(node.getOperator)
    val lhs = getResultSimpleExpr(node.getLeftOperand)
    val rhs = getResultSimpleExpr(node.getRightOperand)
    val loc = mkSourceLoc(node)

    if lhs == null || rhs == null then
      throw new Error(s"lhs or rhs is null at $loc")

    val expr = BinaryExpr(op, lhs, rhs, loc)

    // Make temporary variable if the parent is an expression
    if shouldMakeTemporary(node) then
      val typ = translateType(tb)
      val name = addTempVar(typ, loc, Some(expr))
      saveResult(node, Variable(name, loc))
    else saveResult(node, expr)

  override def endVisit(node: ParenthesizedExpression): Unit =
    val result = getResultSimpleExpr(node.getExpression)
    saveResult(node, result)

  override def endVisit(node: JdtAssignment): Unit =
    // Now we support assignment only to a variable,
    // ignoring assignment to an array element or a field.
    val loc = mkSourceLoc(node)

    def translateOpAssign(op: BinaryOp, name: String): Unit =
      // Example
      // a += b
      // ->
      // a = a + b
      // return a
      val rhs = getResultSimpleExpr(node.getRightHandSide)
      val variable = Variable(name, loc)
      val expr = BinaryExpr(op, variable, rhs, loc)
      addStmt(Assignment(name, expr, loc))
      saveResult(node, variable.copy())

    node.getLeftHandSide match
      case sn: SimpleName =>
        val name = sn.getIdentifier
        if node.getOperator == JdtAssignment.Operator.PLUS_ASSIGN then
          translateOpAssign(BinaryOp.Plus, name)
        else if node.getOperator == JdtAssignment.Operator.MINUS_ASSIGN then
          translateOpAssign(BinaryOp.Minus, name)
        else if node.getOperator == JdtAssignment.Operator.TIMES_ASSIGN then
          translateOpAssign(BinaryOp.Times, name)
        else if node.getOperator == JdtAssignment.Operator.DIVIDE_ASSIGN then
          translateOpAssign(BinaryOp.Divide, name)
        else if node.getOperator == JdtAssignment.Operator.ASSIGN then
          val rhs = getResultExpr(node.getRightHandSide)
          addStmt(Assignment(name, rhs, loc))
        else
          val expr = UnsupportedConstruct(loc)
          saveResult(node, expr)

  override def visit(node: JdtArrayCreation): Boolean =
    // For now, only arrays of single dimension are supported.
    val sizeNode = node.dimensions().get(0).asInstanceOf[ASTNode]
    sizeNode.accept(this)
    val sizeExpr = getResultSimpleExpr(sizeNode)
    val elementType = translateType(node.getType.getElementType)
    val loc = mkSourceLoc(node)
    val expr = ArrayCreation(sizeExpr, elementType, loc)
    saveResult(node, expr)
    false

  override def visit(node: ArrayInitializer): Boolean =
    // For now, we just translate it to array creation
    // without initialization of elements
    if node.getParent.getNodeType != ASTNode.ARRAY_CREATION then
      val size = node.expressions().size()
      val loc = mkSourceLoc(node)
      val elementType = translateType(node.resolveTypeBinding().getElementType)
      val sizeExpr = IntLiteral(size, loc)
      val expr = ArrayCreation(sizeExpr, elementType, loc)
      saveResult(node, expr)
    false

  override def visit(node: SimpleName): Boolean =
    translateVariable(node)
    false

  override def visit(node: QualifiedName): Boolean =
    translateVariable(node)
    false

  override def visit(node: FieldAccess): Boolean =
    val binding = node.resolveFieldBinding()
    if binding != null then
      translateVariable(node, binding)
    false

  override def visit(node: NumberLiteral): Boolean =
    val tb = node.resolveTypeBinding()
    val constExpr = node.resolveConstantExpressionValue()

    if tb.getName == "int" then
      val e = IntLiteral(constExpr.asInstanceOf[Int], mkSourceLoc(node))
      saveResult(node, e)

    // TODO: handle other literals type, like double and strings.
    // else if tb.getName == "double" then
    // val e = DoubleLiteral(constExpr.asInstanceOf[Double], mkSourceLoc(node))
    // saveResult(node, e)

    false

// override def visit(node: Reference): Boolean =

end Visitor
