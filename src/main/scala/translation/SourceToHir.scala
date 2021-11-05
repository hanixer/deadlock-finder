package deadlockFinder
package translation

import hir.*

import org.eclipse.jdt.core.dom.{Type as TypeNode, Block as BlockNode, 
  Assignment as AssignmentNode, *}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.Stack

/** 
  * Translates java source code in the form of Eclipse JDT AST to HIR
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
  val tempVarStack: Stack[ListBuffer[VarDecl]] = Stack()
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
    val b = meth.resolveBinding()
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

  def translateType(typeNode: TypeNode): Type =
    val tb = typeNode.resolveBinding()
    translateType(tb)

  def translateType(tb: ITypeBinding): Type =    
    if tb != null then
      if tb.isPrimitive then tb.getName match 
        case "int" => IntType()
        case "Double" => DoubleType()
        case _ => VoidType()
      else ClassType()
    else
      println(s"Unresolved binding")
      VoidType()

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
    tempVarStack.top += VarDecl(name, typ, rhs, loc)
    name

  override def visit(node: CompilationUnit): Boolean =
    println("Compilation unit is here!")
    compilationUnit = node
    true

  override def visit(node: TypeDeclaration): Boolean =
    println("Here is a type: " + mkFullName(node.resolveBinding()))
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
    tempVarStack.push(ListBuffer())
    true

  override def endVisit(node: BlockNode): Unit =
    tempVarStack.pop()

    val stmts = node.statements.asScala.toList
      .filter(s => s != null)
      .map(s => s.asInstanceOf[Statement])
      .map(s => s.getProperty(TranslateProperty).asInstanceOf[Stmt])
    
    node.setProperty(TranslateProperty, Block(stmts, mkSourceLoc(node)))

  override def visit(node: VariableDeclarationStatement): Boolean =
    println("And here's variable declaration!!!")
    true
    

  override def endVisit(node: ExpressionStatement): Unit =
    val lo = node.getLocationInParent
    val exprHir = node.getExpression.getProperty(TranslateProperty).asInstanceOf[Expr]

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

  override def endVisit(node: InfixExpression): Unit =
    val tb = node.resolveTypeBinding

    // Make temporary variable if the parent is an expression
    val shouldMkTemp =
      val p = node.getParent
      if p.isInstanceOf[Expression] then true
      else false

    val op = translateOperator(node.getOperator)
    val lhs = node.getLeftOperand.getProperty(TranslateProperty).asInstanceOf[SimpleExpr]
    val rhs = node.getRightOperand.getProperty(TranslateProperty).asInstanceOf[SimpleExpr]
    val loc = mkSourceLoc(node)
    val expr = BinaryOpExpr(op, lhs, rhs, loc)

    if shouldMkTemp then
      var typ = translateType(node.resolveTypeBinding)
      var name = addTempVar(typ, loc, Some(expr))
      node.setProperty(TranslateProperty, Variable(name, loc))
    else
      node.setProperty(TranslateProperty, expr)


end Visitor
