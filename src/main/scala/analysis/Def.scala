package deadlockFinder
package analysis

import hir.{AbstractVar, BinaryExpr, CallExpr, Expr, IntLiteral, SimpleExpr, UnaryExpr, Variable}
import lil.*
import common.*

trait Def:
  val varInfo: VarInfo

object Def:
  case class VDecl(varInfo: VarInfo, decl: VarDecl) extends Def:
    override def toString: String = s"Def.VDecl($varInfo)"
  case class Assign(varInfo: VarInfo, assign: Assignment) extends Def:
    override def toString: String = s"Def.Assign($varInfo)"
  case class BParam(varInfo: VarInfo, label: String, index: Int) extends Def:
    override def toString: String = s"Def.BParam($varInfo, $label, $index)"
  case class FParam(varInfo: VarInfo, index: Int) extends Def:
    override def toString: String = s"Def.FParam($varInfo, $index)"
  case class Undefined(varInfo: VarInfo) extends Def:
    override def toString: String = s"Def.Undefined($varInfo)"

  def apply(vd: VarDecl): VDecl = VDecl(VarInfo(vd.v), vd)
  def apply(a: Assignment): Assign = Assign(VarInfo(a.lhs), a)
