package deadlockFinder
package hir

import org.typelevel.paiges.Doc
import deadlockFinder.hir.PrettyPrint

object PrettyPrint:
  def apply(prog: Program, width: Int = 80): String =
    val first = func(prog.funcs(0))
    first.render(0)

  // <FuncDecl> ::= func <name>(<name>: <typ>, ...): <type> { <body> }
  def func(f: FuncDecl): Doc =
    val params = f.params.map(p => Doc.text(p.name) :+ ": " + Doc.str(p.typ))
    val paramsComma = separateComma(params)
    val body = stmt(f.body)
    ("func " +: Doc.text(f.name))
      + ("(" +: paramsComma :+ "): ")
      + Doc.str(f.retTyp) + Doc.space + body

  def stmt(s: Stmt): Doc = s match
    case b: Block =>
      val body = Doc.fill(Doc.line, b.stmts.map(stmt))
      ("{" +: Doc.line) + body + (Doc.line :+ "}")

    case a: Assignment => Doc.text(a.lhs) :+ " = " + expr(a.rhs)

    case vd: VarDecl =>
      var rhs = vd.rhs match
        case Some(e) => " = " +: expr(e)
        case _       => Doc.empty
      ("var " + vd.name + ": ") +: (Doc.str(vd.t) + rhs)

    case _ => Doc.text("unsupported")

  def expr(e: Expr): Doc = e match
    case i: IntLiteral => Doc.str(i.n)
    case v: Variable   => Doc.text(v.name)
    case bop: BinaryOpExpr =>
      expr(bop.lhs) + Doc.str(bop.op) + expr(bop.rhs)
    case c: CallExpr =>
      val args = separateComma(c.args.map(expr))
      Doc.str(c) + ("(" +: args :+ ")")

  def separateComma(ds: List[Doc]): Doc = Doc.fill(Doc.text(", "), ds)

end PrettyPrint
