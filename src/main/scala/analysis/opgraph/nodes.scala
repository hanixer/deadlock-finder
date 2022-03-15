package deadlockFinder
package analysis.opgraph

import analysis.ProcessRank

trait Node

class IntermediateNode(val label: String) extends Node:
  override def toString: String = s"Intermediate $label"

trait CallNode extends Node:
  def sender: ProcessRank
  def receiver: Int

class SendNode(val sender: ProcessRank, val receiver: Int) extends CallNode:
  override def toString: String = s"Send $sender => $receiver"

class RecvNode(val sender: ProcessRank, val receiver: Int) extends CallNode:
  override def toString: String = s"Recv $sender <= $receiver"
