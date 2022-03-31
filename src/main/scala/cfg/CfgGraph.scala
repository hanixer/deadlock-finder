package deadlockFinder
package cfg

import common.Graph
import lil.*

import scala.collection.mutable

/** Represents control-flow graph (CFG) of a function.
  */
class CfgGraph(
    val entry: String,
    val exit: String,
    edges: List[(String, String)]
) extends Graph(edges)

object CfgGraph:
  def apply(func: FuncDecl): CfgGraph =
    val edges = func.body.flatMap { b =>
      b.transfer match
        case j: Jump => List((b.label, j.label))
        case j: CondJump =>
          List(
            (b.label, j.thenLabel),
            (b.label, j.elseLabel)
          )
        case _ => Nil
    }
    new CfgGraph(func.entryLabel, func.exitLabel, edges)

  def apply(
      nodes: List[String],
      edges: List[(String, String)],
      entry: String,
      exit: String
  ): CfgGraph =
    new CfgGraph(entry, exit, edges)
