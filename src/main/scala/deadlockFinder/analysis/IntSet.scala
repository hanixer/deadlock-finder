package deadlockFinder
package analysis

// Examples of sets:
// p = 0
// p != 0 && p == 1
// p != 0 && p != 1 && p == 2

// Sets should be serialized to petri nets later.
// They are bounded by range [0, MAX_INT]
// Later we can think about handling sets consisting of only even or only odd integers.
// For now, main use cases are: singleton set or intersection of NOT Singletons (for example p != 0).
//

/**
 * Hello
 */
abstract class IntSet:
  def contains(n: Int): Boolean
  def include(n: Int): IntSet
  def except(n: Int): IntSet
  def union(set: IntSet): IntSet
  def intersection(set: IntSet): IntSet


//class Singleton extends IntSet:
