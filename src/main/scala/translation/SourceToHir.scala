package deadlockFinder
package translation

import hir.{FuncDecl, Program, VoidType}

import org.eclipse.jdt.core.dom.{ASTNode, ASTVisitor, CompilationUnit, ITypeBinding, MethodDeclaration, PackageDeclaration, SingleVariableDeclaration, TypeDeclaration}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

/**
 * Translates java source code in the form of Eclipse JDT AST
 * to HIR (High-level intermediate representation)
 */
object SourceToHir:
  def apply(cu: CompilationUnit): Program =
    cu.accept(Visitor)
    Program(List())

private object Visitor extends ASTVisitor :
  val TranslateProperty = "Translate"
  var compilationUnit: CompilationUnit = null

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
      if tb != null then
        s"${mkFullName(b.getDeclaringClass)}.$n"
      else n
    else n

  def mkSourceLoc(node: ASTNode): SourceLoc =
    val pos = node.getStartPosition
    SourceLoc(compilationUnit.getLineNumber(pos), compilationUnit.getColumnNumber(pos) + 1)

  override def visit(node: CompilationUnit): Boolean =
    println("Compilation unit is here!")
    compilationUnit = node
    true

  override def visit(node: TypeDeclaration): Boolean =
    println("Here is a type: " + mkFullName(node.resolveBinding()))

    true

  override def endVisit(node: TypeDeclaration): Unit =
    val funcs: ListBuffer[FuncDecl] = ListBuffer()
    val methods = node.getMethods
    for m <- methods do
      val prop = m.getProperty(TranslateProperty)
      if prop != null then
        val func = prop.asInstanceOf[FuncDecl]
        funcs += func
    node.setProperty(TranslateProperty, funcs.toList)

  override def visit(node: MethodDeclaration): Boolean =
    println(s"resolve method: ${mkFullName(node)}, loc: ${mkSourceLoc(node)}")

    // Gather parameters
    val params = node.parameters().asScala.map(p => p.asInstanceOf[SingleVariableDeclaration])

    // Gather return type
    // Translate body


    node.getBody.accept(this)
    false

end Visitor
