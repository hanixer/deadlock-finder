package deadlockFinder
package common

import org.typelevel.paiges.Doc

object PrettyPrint:
  def apply(node: AstNode): String =
    node.prettyPrint.render(0)

  def separateComma(ds: List[Doc]): Doc =
    Doc.fill(Doc.text(", "), ds)
