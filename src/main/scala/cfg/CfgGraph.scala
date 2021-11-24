package deadlockFinder
package cfg

import lil.*
import scala.collection.mutable

/** Represents controlflow graph (CFG) of a function.
  */
class CfgGraph(val preds: Map[String, List[String]], succs: Map[String, List[String]]):

  /** Returns a list of predecessors of the given node. */
  def getPreds(node: String): List[String] =
    preds(node)

  /** Returns a list of successors of the given node. */
  def getSuccs(node: String): List[String] =
    succs(node)

object CfgGraph:
  def apply(func: FuncDecl): CfgGraph =
    type NodesMap = mutable.Map[String, mutable.ListBuffer[String]]

    val preds, succs: NodesMap = mutable.Map()

    def getList(map: NodesMap, k: String): mutable.ListBuffer[String] =
      val v = map.get(k)
      if v.isEmpty then
        val l = mutable.ListBuffer[String]()
        map(k) = l
        l
      else v.get

    def addEdge(from: String, to: String): Unit =
      getList(preds, to).addOne(from)
      getList(succs, from).addOne(to)

    for b <- func.body do
      b.transfer match
        case j: Jump => addEdge(b.label, j.label)
        case j: CondJump =>
          addEdge(b.label, j.thenLabel)
          addEdge(b.label, j.elseLabel)
        case _ =>
    
    def toImmutable(map: NodesMap) = 
      map.map((k,v) => (k, v.toList)).toMap

    new CfgGraph(toImmutable(preds), toImmutable(succs))
