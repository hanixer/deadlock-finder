package deadlockFinder
package analysis.operation.graph

import analysis.ProcessRank

trait Node

class Intermediate extends Node

class SendNode(val sender: ProcessRank, val receiver: Int) extends Node

class RecvNode(val sender: ProcessRank, val receiver: Int) extends Node
