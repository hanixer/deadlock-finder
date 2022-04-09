package deadlockFinder
package analysis.opgraph

import analysis.ProcessRank

trait Node

class IntermediateNode(val label: String) extends Node:
  override def toString: String = s"Intermediate $label"

class SplitNode(val label: String = "") extends Node:
  override def toString: String = s"Split $label"

class MergeNode(val label: String = "") extends Node:
  override def toString: String = s"Merge $label"
  
trait CallNode extends Node

class SendNode(val caller: ProcessRank, val callee: Int, val isSendSource: Boolean = false) extends CallNode:
  override def toString: String = s"Send $caller => $callee"

class RecvNode(val caller: ProcessRank, val callee: ProcessRank) extends CallNode:
  override def toString: String = s"Recv $caller <= $callee"

type Edge = (Node, Node)