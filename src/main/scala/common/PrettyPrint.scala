package deadlockFinder
package common

import org.typelevel.paiges.Doc
import cfg.CfgGraph
import deadlockFinder.lil.FuncDecl

object PrettyPrint:
  def apply(node: AstNode): String =
    node.prettyPrint.render(0)

  def cfgToDot(cfg: CfgGraph): String =
    "digraph G {\n" + cfg.getAllNodes.flatMap(n => cfg.getSuccs(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"

  def funcToDot(func: FuncDecl, cfg: CfgGraph): String =
    val blocks = func.body.map(b => 
      val elems = b.label :: b.stmts.map(PrettyPrint.apply).appended(PrettyPrint(b.transfer))      
      s"${b.label} [label=\"${elems.mkString("\n")}\"]").mkString("\n") + "\n"
    "digraph G {\n" + blocks + cfg.getAllNodes.flatMap(n => cfg.getSuccs(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"    
  
  def separateComma(ds: List[Doc]): Doc =
    Doc.fill(Doc.text(", "), ds)
