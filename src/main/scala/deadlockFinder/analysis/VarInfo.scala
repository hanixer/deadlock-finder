package deadlockFinder
package analysis

import expr.*
import lil.*
import common.*

case class VarInfo(name: String, index: Option[Int]):
    override def toString: String = name + (index.map(i => "." + i).getOrElse(""))
  
object VarInfo:
  def apply(v: AbstractVar): VarInfo = v match
    case SsaVariable(n, i, _) => VarInfo(n, Some(i))
    case Variable(n, _) => VarInfo(n, None)

  def apply(name: String): VarInfo = VarInfo(name, None)
