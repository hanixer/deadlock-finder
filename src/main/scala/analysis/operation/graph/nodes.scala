package deadlockFinder
package analysis.operation.graph

import analysis.ProcessPredicate

trait Node

class Intermediate extends Node

//class SendNode(val sender: Sender, val receiver: Int) extends Node
//class RecvNode(val sender: Set[ProcessPredicate], val receiver: Int) extends Node
