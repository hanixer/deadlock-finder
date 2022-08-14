package deadlockFinder
package analysis.opgraph

import common.Graph

class OperationGraph(val root: Node, edges: List[Edge]) extends Graph[Node](edges)

