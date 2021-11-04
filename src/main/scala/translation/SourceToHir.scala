package deadlockFinder
package translation

import hir.Program

import org.eclipse.jdt.core.dom.{ASTVisitor, CompilationUnit, ITypeBinding, MethodDeclaration, PackageDeclaration, TypeDeclaration}

/**
 * Translates java source code in the form of Eclipse JDT AST
 * to HIR (High-level intermediate representation)
 */
object SourceToHir:
  def apply(cu: CompilationUnit): Program =
    cu.accept(Visitor)
    Program(List())

private object Visitor extends ASTVisitor :
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

  override def visit(node: CompilationUnit): Boolean =
    println("Compilation unit is here!")
    true

  override def visit(node: TypeDeclaration): Boolean =
    println("Here is a type: " + mkFullName(node.resolveBinding()))

    true

  override def visit(node: MethodDeclaration): Boolean =
    println(s"resolve method: ${mkFullName(node)}")
    true

  override def endVisit(node: TypeDeclaration): Unit =
    val methods = node.getMethods

end Visitor
