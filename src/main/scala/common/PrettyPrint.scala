package deadlockFinder
package common

import org.typelevel.paiges.Doc
import cfg.CfgGraph
import deadlockFinder.lil.FuncDecl
import deadlockFinder.hil.Variable
import deadlockFinder.hil.AbstractVar

object PrettyPrint:
  def apply(node: AstNode): String =
    node.prettyPrint.render(0)

  def cfgToDot(cfg: CfgGraph): String =
    "digraph G {\n" + cfg.allNodes.flatMap(n => cfg.successors(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"

  def funcToDot(func: FuncDecl, cfg: CfgGraph): String =
    val blocks = func.body.map(b => 
      val elems = b.label :: b.stmts.map(PrettyPrint.apply).appended(PrettyPrint(b.transfer))      
      s"${b.label} [label=\"${elems.mkString("\n")}\"]").mkString("\n") + "\n"
    "digraph G {\n" + blocks + cfg.allNodes.flatMap(n => cfg.successors(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"    
  
  def separateComma(ds: List[Doc]): Doc =
    Doc.fill(Doc.text(", "), ds)

  def inParens(d: Doc): Doc =
    "(" +: d :+ ")"

  def inParensAndComma(ds: List[Doc]): Doc =
    inParens(separateComma(ds))

  def argsOrEmpty(vars: List[AbstractVar]): Doc =    
    if vars.isEmpty then Doc.empty
    else PrettyPrint.inParensAndComma(vars.map(_.prettyPrint))