package deadlockFinder
package hir

import org.typelevel.paiges.Doc

object PrettyPrint {
  def apply(prog: Program, width: Int = 80): String =
    def first = func(prog.funcs(0))
    first.render(width)
  
  // <FuncDecl> ::= func <name>(<name>: <typ>, ...): <type> { <body> }
  def func(f: FuncDecl): Doc =  
    val params = f.params.map(p => Doc.text(p.name) :+ ": " + Doc.str(p.typ))
    val paramsComma = Doc.fill(Doc.text(", "), params) 
    "func" +: Doc.str(f.name) :+ "(" + paramsComma :+ "): " + Doc.str(f.retTyp) + stmt(f.body)

  def stmt(s: Stmt): Doc = s match
    case b: Block => "{" +: Doc.fill(Doc.line, b.stmts.map(stmt)) :+ "}"
    case a: Assignment => Doc.text(a.lhs) :+ " = "
    case _ => Doc.text("unsupported")
    
  def expr(e: Expr): Doc = e match
    case i: IntLiteral => Doc.str(i.n)
    case v: Variable => Doc.text(v.name)
    case bop: BinaryOpExpr => expr(bop.lhs) + Doc.str(bop.op) + expr(bop.rhs)
    case c: CallExpr => Doc.str(c)
}
