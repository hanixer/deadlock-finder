package deadlockFinder
package analysis

import expr.*
import lil.*
import common.*

trait Use:
  val varInfo: VarInfo

object Use:
  case class Assign(varInfo: VarInfo, assign: Assignment) extends Use:
    override def toString: String = s"Use.Assign($varInfo in ${assign.lhs})"
  case class BParam(varInfo: VarInfo, label: String, index: Int) extends Use:
    override def toString: String = s"Use.BParam($varInfo, $label, $index)"
