package deadlockFinder
package analysis.opgraph

import analysis.ProcessRank

trait Node

class IntermediateNode(val label: String) extends Node:
  override def toString: String = s"Intermediate $label"

trait CallNode extends Node

class SendNode(val caller: ProcessRank, val callee: Int) extends CallNode:
  override def toString: String = s"Send $caller => $callee"

class RecvNode(val caller: ProcessRank, val callee: ProcessRank) extends CallNode:
  override def toString: String = s"Recv $caller <= $callee"

type Edge = (Node, Node)