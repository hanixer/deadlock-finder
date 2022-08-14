package deadlockFinder
package common

import org.typelevel.paiges.Doc

case class SourceLoc(line: Int, column: Int)

abstract class AstNode:
  val loc: SourceLoc
  def prettyPrint: Doc

case class Param(name: String, typ: Type, loc: SourceLoc) extends AstNode:
  def prettyPrint: Doc = (name + ": ") +: Doc.str(typ)
