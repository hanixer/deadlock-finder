package deadlockFinder
package cfg

import lil.*

import scala.collection.mutable

/** Represents controlflow graph (CFG) of a function.
  */
class CfgGraph(
    preds: Map[String, List[String]],
    succs: Map[String, List[String]],
    val entry: String
):

  /** Returns a list of predecessors of the given node. */
  def getPreds(node: String): List[String] =
    preds(node)

  /** Returns a list of successors of the given node. */
  def getSuccs(node: String): List[String] =
    succs(node)

  def getAllNodes: List[String] = preds.keys.toList

object CfgGraph:
  type BuilderMap = mutable.Map[String, mutable.ListBuffer[String]]
  def toImmutable(map: BuilderMap): Map[String, List[String]] =
    map.map((k, v) => (k, v.toList)).toMap

  class Builder(nodes: List[String]):
    val preds, succs: BuilderMap = mutable.Map()
    for n <- nodes do
      preds(n) = mutable.ListBuffer()
      succs(n) = mutable.ListBuffer()

    def addEdge(from: String, to: String): Unit =
      preds(to).addOne(from)
      succs(from).addOne(to)

    def getPreds: Map[String, List[String]] = toImmutable(preds)
    def getSuccs: Map[String, List[String]] = toImmutable(succs)

  // TODO: refactor
  def apply(func: FuncDecl): CfgGraph =

    val preds, succs: BuilderMap = mutable.Map()

    def addEdge(from: String, to: String): Unit =
      preds(to).addOne(from)
      succs(from).addOne(to)

    // Initialize lists
    for b <- func.body do
      preds(b.label) = mutable.ListBuffer()
      succs(b.label) = mutable.ListBuffer()

    // Create edges for each block
    for b <- func.body do
      b.transfer match
        case j: Jump => addEdge(b.label, j.label)
        case j: CondJump =>
          addEdge(b.label, j.thenLabel)
          addEdge(b.label, j.elseLabel)
        case _ =>

    new CfgGraph(toImmutable(preds), toImmutable(succs), func.body.head.label)

  def apply(
      nodes: List[String],
      edges: List[(String, String)],
      entry: String
  ): CfgGraph =
    val builder = new Builder(nodes)
    for (from, to) <- edges do builder.addEdge(from, to)
    new CfgGraph(builder.getPreds, builder.getSuccs, entry)
