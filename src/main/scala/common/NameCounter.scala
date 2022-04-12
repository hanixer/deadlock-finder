package deadlockFinder
package common

/** Generates new names based on prefix. */
class NameCounter(prefix: String = "t~"):
  var counter = 0

  def newName(prefix: String): String =
    counter += 1
    prefix + counter

  def newName(): String = newName(prefix)

