package deadlockFinder
package analysis.operation.graph

import analysis.ProcessRank

trait Node

class IntermediateNode(val label: String) extends Node:
  override def toString: String = s"Intermediate $label"

class SendNode(val sender: ProcessRank, val receiver: Int) extends Node:
  override def toString: String = s"Send $sender => $receiver"

class RecvNode(val sender: ProcessRank, val receiver: Int) extends Node:
  override def toString: String = s"Recv $sender <= $receiver"
