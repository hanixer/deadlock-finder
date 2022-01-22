package deadlockFinder
package translation

import common.*
import hir.*

import org.eclipse.jdt.core.dom.{Assignment as AssignmentNode, Block as BlockNode, Type as TypeNode, *}

import javax.swing.tree.TreePath
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}
import scala.jdk.CollectionConverters.*

/** Translates java source code in the form of Eclipse JDT AST to HIR
  * (High-level intermediate representation)
  */
object SourceToHir:
  def apply(cu: CompilationUnit): Program =
    val visitor = new Visitor
    cu.accept(visitor)
    Program(visitor.getFuncs)

class Visitor extends ASTVisitor:
  private val TranslateProperty = "Translate"
  private var compilationUnit: CompilationUnit = null
  private val funcs = ListBuffer[FuncDecl]()
  private val stmtsStack: mutable.Stack[ListBuffer[(Stmt, SourceLoc)]] = mutable.Stack()
  private var tempCounter = 0

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
    else if p.getNodeType == ASTNode.WHILE_STATEMENT then true
    else if p.getNodeType == ASTNode.RETURN_STATEMENT then true
    else false

  def translateType(typeNode: TypeNode): Type =
    val tb = typeNode.resolveBinding()
    translateType(tb)

  def translateType(tb: ITypeBinding): Type =
    if tb != null then
      if tb.isPrimitive then
        tb.getName match
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
    if binding.isField then
      if Modifier.isStatic(binding.getModifiers) then
        val className = mkFullName(binding.getDeclaringClass)
        val sfa = StaticFieldAccess(className, name, loc)
        node.setProperty(TranslateProperty, sfa)
    else
      val v = Variable(name, loc)
      node.setProperty(TranslateProperty, v)

  def addTempVar(typ: Type, loc: SourceLoc, rhs: Option[Expr] = None): String =
    tempCounter += 1
    val name = s"t~$tempCounter"
    addStmt(VarDecl(name, typ, rhs, loc))
    name

  def pushStmtStack(): Unit =
    stmtsStack.push(ListBuffer())

  def popStmtStack(): List[(Stmt, SourceLoc)] =
    stmtsStack.pop().toList

  def addStmt(stmt: Stmt): Unit =
    stmtsStack.top.addOne(stmt, stmt.loc)

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
    compilationUnit = node
    true

  override def endVisit(node: TypeDeclaration): Unit =
    // val funcs: ListBuffer[FuncDecl] = ListBuffer()
    val methods = node.getMethods
    for m <- methods do
      val prop = m.getProperty(TranslateProperty)
      if prop != null then
        val func = prop.asInstanceOf[FuncDecl]
        funcs += func
    node.setProperty(TranslateProperty, funcs.toList)

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

    node.setProperty(TranslateProperty, func)

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

  override def visit(node: WhileStatement): Boolean =
    pushStmtStack()
    // Condition
    node.getExpression.accept(this)
    val cond = getResultSimpleExpr(node.getExpression)
    val condLoc = mkSourceLoc(node.getExpression)
    val condNot = UnaryExpr(UnaryOp.Not, cond, condLoc)
    val condVar =
      Variable(addTempVar(BooleanType, condLoc, Some(condNot)), condLoc)
    addStmt(IfThenElse(condVar, Break(condLoc), None, condLoc))

    // Body
    val bodyStmts = translateBody(node.getBody)
    bodyStmts.foreach(addStmt) // Add all statements to the current level

    val loc = mkSourceLoc(node)

    val stmts = popStmtStack().map(_._1)

    addStmt(Loop(Block(stmts, loc), loc))

    false

  override def endVisit(node: VariableDeclarationStatement): Unit =
    val typ = translateType(node.getType)

    val varDecls = node.fragments.asScala.toList.map(f =>
      val frag = f.asInstanceOf[VariableDeclarationFragment]
      val name = frag.getName.getIdentifier
      val rhs = Option(frag.getInitializer).map(getResultExpr)
      val loc = mkSourceLoc(frag)
      (VarDecl(name, typ, rhs, loc), loc)
    )

    stmtsStack.top.appendAll(varDecls)

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
    val expr = node.getExpression

    val binding = node.resolveMethodBinding
    if Modifier.isStatic(binding.getModifiers) then
      val args = node.arguments.asScala.toList.map(a =>
        getResultSimpleExpr(a.asInstanceOf[Expression])
      )
      val name = mkFullName(binding, node.getName.getIdentifier)
      val loc = mkSourceLoc(node)
      val expr = CallExpr(name, args, loc)

      // Make temporary variable if the parent is an expression
      if shouldMakeTemporary(node) then
        val typ = translateType(node.resolveTypeBinding)
        val name = addTempVar(typ, loc, Some(expr))
        node.setProperty(TranslateProperty, Variable(name, loc))
      else node.setProperty(TranslateProperty, expr)
    else println("Non-static methods are not implemented")

  override def endVisit(node: PrefixExpression): Unit =
    // TODO: handle increment/decrement
    val loc = mkSourceLoc(node)
    val expr =
      if node.getOperator == PrefixExpression.Operator.NOT then
        val op = UnaryOp.Not
        val e = getResultSimpleExpr(node.getOperand)
        UnaryExpr(op, e, loc)
      else
        val op =
          if node.getOperator == PrefixExpression.Operator.MINUS then
            BinaryOp.Minus
          else BinaryOp.Plus

        val lhs = IntLiteral(0, loc)
        val rhs = getResultSimpleExpr(node.getOperand)
        BinaryExpr(op, lhs, rhs, loc)

    val typ = translateType(node.resolveTypeBinding)
    val name = addTempVar(typ, loc, Some(expr))

    node.setProperty(TranslateProperty, Variable(name, mkSourceLoc(node)))

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
      node.setProperty(TranslateProperty, Variable(name, loc))
    else node.setProperty(TranslateProperty, expr)

  override def endVisit(node: AssignmentNode): Unit =
    // Now we support assignment only to a variable,
    // ignoring assignment to an array element or a field.
    node.getLeftHandSide match
      case sn: SimpleName =>
        val name = sn.getIdentifier
        val rhs = getResultExpr(node.getRightHandSide)
        val loc = mkSourceLoc(node)
        addStmt(Assignment(name, rhs, loc))

  override def visit(node: SimpleName): Boolean =
    val binding = node.resolveBinding()
    if binding != null && binding.getKind == IBinding.VARIABLE then
      translateVariable(node, binding.asInstanceOf[IVariableBinding])
    false

  override def visit(node: FieldAccess): Boolean =
    val binding = node.resolveFieldBinding()
    if binding != null then
      if Modifier.isStatic(binding.getModifiers) then
        translateVariable(node, binding)
      else println("Non-static fields are not supported")
    false

  override def visit(node: NumberLiteral): Boolean =
    val tb = node.resolveTypeBinding()
    val constExpr = node.resolveConstantExpressionValue()

    if tb.getName == "int" then
      val e = IntLiteral(constExpr.asInstanceOf[Int], mkSourceLoc(node))
      node.setProperty(TranslateProperty, e)

    // TODO: handle other literals type, like double and strings.
    // else if tb.getName == "double" then
    // val e = DoubleLiteral(constExpr.asInstanceOf[Double], mkSourceLoc(node))
    // node.setProperty(TranslateProperty, e)

    false

// override def visit(node: Reference): Boolean =

end Visitor
