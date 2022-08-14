package deadlockFinder
package expr

import common.{AstNode, PrettyPrint, SourceLoc, Type}

import org.typelevel.paiges.Doc

trait Expr extends AstNode

trait SimpleExpr extends Expr

abstract class AbstractVar extends SimpleExpr:
  def name: String

case class Variable(name: String, loc: SourceLoc) extends AbstractVar:
  def prettyPrint: Doc = Doc.text(name)

case class StaticFieldAccess(className: String, fieldName: String, loc: SourceLoc) extends SimpleExpr:
  def prettyPrint: Doc = Doc.text(s"$className.$fieldName")

case class FieldAccess(instance: SimpleExpr, fieldName: String, loc: SourceLoc) extends SimpleExpr:
  def prettyPrint: Doc = instance.prettyPrint + Doc.text(s".$fieldName")

case class IntLiteral(n: Int, loc: SourceLoc) extends SimpleExpr:
  def prettyPrint: Doc = Doc.str(n)

case class BoolLiteral(b: Boolean, loc: SourceLoc) extends SimpleExpr:
  def prettyPrint: Doc = Doc.str(b)

case class BinaryExpr(
    op: BinaryOp,
    lhs: SimpleExpr,
    rhs: SimpleExpr,
    loc: SourceLoc
) extends Expr:
  def prettyPrint: Doc = lhs.prettyPrint + Doc.str(op) + rhs.prettyPrint

case class UnaryExpr(op: UnaryOp, e: SimpleExpr, loc: SourceLoc) extends Expr:
  def prettyPrint: Doc = Doc.str(op) + e.prettyPrint

case class CallExpr(name: String, args: List[SimpleExpr], loc: SourceLoc) extends Expr:
  def prettyPrint: Doc =
    val a = PrettyPrint.inParensAndComma(args.map(_.prettyPrint))
    Doc.text(name)

case class ArrayCreation(sizeExpr: SimpleExpr, elementType: Type, loc: SourceLoc) extends Expr:
  override def prettyPrint: Doc =
    Doc.text(s"new $elementType[") + sizeExpr.prettyPrint + Doc.text("]")
