package deadlockFinder
package translation

import hir.*

import org.eclipse.jdt.core.dom.{
  Type as TypeNode,
  Block as BlockNode,
  Assignment as AssignmentNode,
  *
}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.Stack
import javax.swing.tree.TreePath

/** Translates java source code in the form of Eclipse JDT AST to HIR
  * (High-level intermediate representation)
  */
object SourceToHir:
  def apply(cu: CompilationUnit): Program =
    cu.accept(Visitor)
    Program(Visitor.getFuncs)

private object Visitor extends ASTVisitor:
  val TranslateProperty = "Translate"
  var compilationUnit: CompilationUnit = null
  private val funcs = ListBuffer[FuncDecl]()
  val stmtsStack: Stack[ListBuffer[Stmt]] = Stack()
  var tempCounter = 0

  def getFuncs: List[FuncDecl] = funcs.toList

  def mkFullName(typ: TypeDeclaration): String =
    val b = typ.resolveBinding()
    if b != null then mkFullName(b)
    else typ.getName.getIdentifier

  def mkFullName(b: ITypeBinding) =
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
    if p.isInstanceOf[Expression] && p.getNodeType != ASTNode.ASSIGNMENT then true
    else if p.getNodeType == ASTNode.IF_STATEMENT then true
    else false

  def translateType(typeNode: TypeNode): Type =
    val tb = typeNode.resolveBinding()
    translateType(tb)

  def translateType(tb: ITypeBinding): Type =
    if tb != null then
      if tb.isPrimitive then
        tb.getName match
          case "int"    => IntType
          case "double" => DoubleType
          case "float" => FloatType
          case "boolean" => BooleanType
          case _        => VoidType
      else ClassType()
    else
      println(s"Unresolved binding")
      VoidType

  def translateOperator(op: InfixExpression.Operator): BinaryOp = op match
    case InfixExpression.Operator.PLUS => BinaryOp.Plus
    case InfixExpression.Operator.MINUS => BinaryOp.Minus
    case InfixExpression.Operator.TIMES => BinaryOp.Times
    case InfixExpression.Operator.DIVIDE => BinaryOp.Divide
    case InfixExpression.Operator.LESS => BinaryOp.Less
    case InfixExpression.Operator.GREATER => BinaryOp.Greater
    case InfixExpression.Operator.LESS_EQUALS => BinaryOp.LessEquals
    case InfixExpression.Operator.GREATER_EQUALS => BinaryOp.GreaterEquals
    case InfixExpression.Operator.EQUALS => BinaryOp.Equals
    case InfixExpression.Operator.AND => BinaryOp.And
    case InfixExpression.Operator.OR => BinaryOp.Or
    case _ =>
      println("Unknown operator: $op")
      BinaryOp.Plus

  def addTempVar(typ: Type, loc: SourceLoc, rhs: Option[Expr] = None): String =
    tempCounter += 1
    val name = s"t~$tempCounter"
    addStmt(VarDecl(name, typ, rhs, loc))
    name

  def addStmt(stmt: Stmt): Unit =
    stmtsStack.top += stmt

  def getResultExpr(node: ASTNode): Expr =
    node.getProperty(TranslateProperty).asInstanceOf[Expr]

  def getResultSimpleExpr(node: ASTNode): SimpleExpr =
    node.getProperty(TranslateProperty).asInstanceOf[SimpleExpr]

  def getResultStmt(node: ASTNode): Stmt =
    node.getProperty(TranslateProperty).asInstanceOf[Stmt]

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
    node.getBody.accept(this)
    val body = node.getBody.getProperty(TranslateProperty).asInstanceOf[Block]

    val func =
      FuncDecl(mkFullName(node), params, retType, body, mkSourceLoc(node))
    node.setProperty(TranslateProperty, func)

    false

  override def visit(node: BlockNode): Boolean =
    stmtsStack.push(ListBuffer())
    true

  override def endVisit(node: BlockNode): Unit =
    val stmts = stmtsStack.pop().toList
    node.setProperty(TranslateProperty, Block(stmts, mkSourceLoc(node)))

  override def endVisit(node: IfStatement): Unit =
    val cond = getResultSimpleExpr(node.getExpression)
    val thenStmt = getResultStmt(node.getThenStatement)
    val elseStmt = Option(node.getElseStatement).map(getResultStmt)
    val loc = mkSourceLoc(node)
    addStmt(IfThenElse(cond, thenStmt, elseStmt, loc))

  override def endVisit(node: VariableDeclarationStatement): Unit =
    val typ = translateType(node.getType)

    val varDecls = node.fragments.asScala.toList.map(f =>
      val frag = f.asInstanceOf[VariableDeclarationFragment]
      val name = frag.getName.getIdentifier
      val rhs = Option(frag.getInitializer).map(getResultExpr)
      val loc = mkSourceLoc(frag)
      VarDecl(name, typ, rhs, loc)
    )

    stmtsStack.top.appendAll(varDecls)

  override def endVisit(node: ExpressionStatement): Unit =    
    node.getExpression match
      case mi: MethodInvocation => 
        val e = mi.getProperty(TranslateProperty).asInstanceOf[CallExpr]
        addStmt(CallStmt(e))
      case _ => 

  override def endVisit(node: MethodInvocation): Unit =
    val mb = node.resolveMethodBinding
    if Modifier.isStatic(mb.getModifiers) then
      val args = node.arguments.asScala.toList.map(a =>
        getResultSimpleExpr(a.asInstanceOf[Expression])
      )
      val name = mkFullName(mb, node.getName.getIdentifier)
      val loc = mkSourceLoc(node)
      val expr = CallExpr(name, args, loc)

      // Make temporary variable if the parent is an expression
      if shouldMakeTemporary(node) then
        var typ = translateType(node.resolveTypeBinding)
        var name = addTempVar(typ, loc, Some(expr))
        node.setProperty(TranslateProperty, Variable(name, loc))
      else node.setProperty(TranslateProperty, expr)
      
    else ??? // Non-static methods are not implemented

  override def endVisit(node: InfixExpression): Unit =
    val tb = node.resolveTypeBinding

    val op = translateOperator(node.getOperator)
    val lhs = getResultSimpleExpr(node.getLeftOperand)
    val rhs = getResultSimpleExpr(node.getRightOperand)
    val loc = mkSourceLoc(node)

    if lhs == null || rhs == null then
      throw new Error(s"lhs or rhs is null at $loc")

    val expr = BinaryOpExpr(op, lhs, rhs, loc)

    // Make temporary variable if the parent is an expression
    if shouldMakeTemporary(node) then
      var typ = translateType(tb)
      var name = addTempVar(typ, loc, Some(expr))
      node.setProperty(TranslateProperty, Variable(name, loc))
    else node.setProperty(TranslateProperty, expr)

  override def endVisit(node: AssignmentNode): Unit =
    // Now we support assignment only to a variable, 
    // ignoring assignment to an array element or a field.
    node.getLeftHandSide match
      case sn: SimpleName =>
        val name = sn.getIdentifier
        val rhs = getResultExpr(node.getRightHandSide)
        addStmt(Assignment(name, rhs, mkSourceLoc(node)))

  override def visit(node: SimpleName): Boolean =
    var v = Variable(node.getIdentifier, mkSourceLoc(node))
    node.setProperty(TranslateProperty, v)
    false

  override def visit(node: NumberLiteral): Boolean =
    val tb = node.resolveTypeBinding
    val constExpr = node.resolveConstantExpressionValue

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
