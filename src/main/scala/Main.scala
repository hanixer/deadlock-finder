package deadlockFinder

import hir.{Expr, IntLiteral, Variable}
import translation.SourceToHir

import org.eclipse.jdt.core.dom.*

import java.nio.file.Path

object Main:
  def main(args: Array[String]): Unit =
    val source = java.nio.file.Files.readString(Path.of("examples/showcase/Example2.java"))
//    val source = "package lapack; class Mini { void f2() { f1(\"\"); } void f1(String ooo) {f2();} }"
    val node: CompilationUnit = parseJava(source)
//    val probs = node.getProblems
//    println(probs.mkString("Array(", ", ", ")"))
    println("pass to translation...")
    val res = SourceToHir(node)
    

  private def parseJava(source: String): CompilationUnit =
    val parser = ASTParser.newParser(AST.JLS16)
    parser.setSource(source.toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setResolveBindings(true)
    parser.setEnvironment(Array.empty, Array.empty, Array.empty, true)
    parser.setUnitName("Unit name")
    parser.createAST(null).asInstanceOf[CompilationUnit]

end Main

