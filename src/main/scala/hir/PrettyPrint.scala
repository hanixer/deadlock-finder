package deadlockFinder
package hir

import org.typelevel.paiges.Doc
import deadlockFinder.hir.PrettyPrint

object PrettyPrint:
  def apply(prog: Program, width: Int = 0): String =
    val first = func(prog.funcs(0))
    val res = Doc.fill(Doc.line + Doc.line, prog.funcs.map(func))
    res.render(width)

  // <FuncDecl> ::= func <name>(<name>: <typ>, ...): <type> { <body> }
  def func(f: FuncDecl): Doc =
    val params = f.params.map(p => (p.name + ": ") +: Doc.str(p.typ))
    val paramsComma = separateComma(params)
    val body = stmt(f.body)
    ("func " +: Doc.text(f.name))
      + ("(" +: paramsComma :+ "): ")
      + Doc.str(f.retTyp) + Doc.space + body

  def stmt(s: Stmt): Doc = s match
    case b: Block =>
      val body = Doc.fill(Doc.line, b.stmts.map(stmt))
      (Doc.text("{") + Doc.line + body).nested(2) + (Doc.line :+ "}")

    case a: Assignment => (a.lhs + " = ") +: expr(a.rhs)

    case vd: VarDecl =>
      var rhs = vd.rhs match
        case Some(e) => " = " +: expr(e)
        case _       => Doc.empty
      ("var " + vd.name + ": ") +: (Doc.str(vd.t) + rhs)

    case c: CallStmt =>
      val args = separateComma(c.callExpr.args.map(expr))
      (c.callExpr.name + "(") +: (args :+ ")")
    
    case ite: IfThenElse =>
      val elseBranch = ite.elseStmt match
        case Some(s) => " else " +: stmt(s)
        case _ => Doc.empty
      val ifCond = ("if (" +: expr(ite.cond)) :+ ") "
      ifCond + stmt(ite.thenStmt) + elseBranch

    case l: Loop =>
      Doc.text("loop ") + stmt(l.body)

    case r: Return =>
      "return " +: r.expr.map(expr).getOrElse(Doc.empty)

    case b: Break => Doc.text("break")

    case u: UnsupportedConstruct =>
      Doc.text(s"[unsupportedStmt at ${u.loc}]")

    case _ => Doc.text(s"unsupported statement for printing $s")

  def expr(e: Expr): Doc = e match
    case i: IntLiteral => Doc.str(i.n)
    case v: Variable   => Doc.text(v.name)
    case bop: BinaryOpExpr =>
      expr(bop.lhs) + Doc.str(bop.op) + expr(bop.rhs)
    case c: CallExpr =>
      val args = separateComma(c.args.map(expr))
      Doc.str(c.name) + ("(" +: args :+ ")")
    case u: UnsupportedConstruct =>    
      Doc.text(s"[unsupportedExpr at ${u.loc}]")

  def separateComma(ds: List[Doc]): Doc = Doc.fill(Doc.text(", "), ds)

end PrettyPrint
