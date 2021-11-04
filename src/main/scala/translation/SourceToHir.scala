package deadlockFinder
package translation

import hir.*

import org.eclipse.jdt.core.dom.{Type as TypeNode, Block as BlockNode, *}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

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
  def getFuncs: List[FuncDecl] = funcs.toList

  def mkFullName(typ: TypeDeclaration): String =
    val b = typ.resolveBinding()
    if b != null then mkFullName(b)
    else typ.getName.getIdentifier

  private def mkFullName(b: ITypeBinding) =
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
    val b = typeNode.resolveBinding()
    if b != null then
      if b.isPrimitive then IntType()
      else ClassType()
    else
      println(s"Unresolved binding for $typeNode")
      VoidType()

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

  override def endVisit(node: BlockNode): Unit =
    val stmts = node.statements.asScala.toList
      .filter(s => s != null)
      .map(s => s.asInstanceOf[Statement])
      .map(s => s.getProperty(TranslateProperty).asInstanceOf[Stmt])
    
    node.setProperty(TranslateProperty, Block(stmts, mkSourceLoc(node)))

end Visitor
