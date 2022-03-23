package deadlockFinder
package analysis

enum ProcessRank:
  case AnyRank
  case Concrete(n: Int)

  def toShortString: String = this match
    case AnyRank => "*"
    case Concrete(n) => n.toString
