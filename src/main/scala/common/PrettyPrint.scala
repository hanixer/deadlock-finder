package deadlockFinder
package common

import org.typelevel.paiges.Doc
import cfg.CfgGraph

object PrettyPrint:
  def apply(node: AstNode): String =
    node.prettyPrint.render(0)

  def cfgToDot(cfg: CfgGraph): String =
    "digraph G {\n" + cfg.getAllNodes.flatMap(n => cfg.getSuccs(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"

  def separateComma(ds: List[Doc]): Doc =
    Doc.fill(Doc.text(", "), ds)
