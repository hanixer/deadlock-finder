package deadlockFinder
package common

/** Generates new names based on prefix. */
class NameCounter(var counter: Int = 0, prefix: String = "t~"):
  def newName(prefix: String): String =
    counter += 1
    prefix + counter

  def newName(): String = newName(prefix)

