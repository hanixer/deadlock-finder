package deadlockFinder
package analysis

enum ProcessRank:
  case AnyRank
  case Concrete(n: Int)
