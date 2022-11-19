package deadlockFinder
package analysis

enum ProcessRank:
  case AnyRank // Note, this can be mapped both to ANY_SOURCE and to process, that cannot be determined as concrete
  case Concrete(n: Int)

  override def toString: String = toShortString

  def toShortString: String = this match
    case AnyRank => "*"
    case Concrete(n) => n.toString
    
  def isAnyRank: Boolean = this match
    case AnyRank => true
    case _ => false
