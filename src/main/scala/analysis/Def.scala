package deadlockFinder
package analysis

import expr.*
import lil.*
import common.*

trait Def:
  val varInfo: VarInfo

object Def:
  case class Assign(varInfo: VarInfo, assign: Assignment) extends Def:
    override def toString: String = s"Def.Assign($varInfo)"
  case class BParam(varInfo: VarInfo, label: String, index: Int) extends Def:
    override def toString: String = s"Def.BParam($varInfo, $label, $index)"
  case class FParam(varInfo: VarInfo, index: Int) extends Def:
    override def toString: String = s"Def.FParam($varInfo, $index)"
  case class Undefined(varInfo: VarInfo) extends Def:
    override def toString: String = s"Def.Undefined($varInfo)"

  def apply(a: Assignment): Assign = Assign(VarInfo(a.lhs), a)
