package deadlockFinder
package translation

import deadlockFinder.hir.Program
import org.eclipse.jdt.core.dom.CompilationUnit

/**
 * Translate java source code in the form of Eclipse JDT AST
 * to HIR (High-level intermediate representation)
 */
object SourceToHir {
  def apply(cu: CompilationUnit): Program =
    Program(List())
    
  
}
