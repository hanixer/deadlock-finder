package deadlockFinder
package common

import analysis.opgraph.OperationGraph
import analysis.pnet.{Node, PetriNet, Place, Transition}
import cfg.CfgGraph
import hil.{AbstractVar, Variable}
import lil.FuncDecl

import org.typelevel.paiges.Doc

object PrettyPrint:
  def apply(node: AstNode): String =
    node.prettyPrint.render(0)

  def cfgToDot(cfg: CfgGraph): String =
    "digraph G {\n" + cfg.allNodes.flatMap(n => cfg.successors(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"

  def funcToDot(func: FuncDecl, cfg: CfgGraph): String =
    val blocks = func.body
      .map { b =>
        val elems = b.label :: b.stmts.map(PrettyPrint.apply).appended(PrettyPrint(b.transfer))
        s"${b.label} [label=\"${elems.mkString("\n")}\"]"
      }
      .mkString("\n") + "\n"
    "digraph G {\n" + blocks + cfg.allNodes.flatMap(n => cfg.successors(n).map(s => s"$n -> $s")).mkString("\n") + "\n}"

  def separateComma(ds: List[Doc]): Doc =
    Doc.fill(Doc.text(", "), ds)

  def inParens(d: Doc): Doc =
    "(" +: d :+ ")"

  def inParensAndComma(ds: List[Doc]): Doc =
    inParens(separateComma(ds))

  def argsOrEmpty(vars: List[AbstractVar]): Doc =
    if vars.isEmpty then Doc.empty
    else PrettyPrint.inParensAndComma(vars.map(_.prettyPrint))

  def operationGraphToDot(operationGraph: OperationGraph): String =
    val nodeToIndex = operationGraph.nodes.zipWithIndex.toMap
    val labels = nodeToIndex.toList.map { (n, i) => s"$i [label=\"$n\"]" }
    val edges = operationGraph.edges.map { (n1, n2) =>
      val i1 = nodeToIndex(n1)
      val i2 = nodeToIndex(n2)
      s"$i1 -> $i2"
    }
    "digraph G {\n" +
      labels.mkString("\n") + "\n" +
      edges.mkString("\n") + "\n" +
      "}"

  def petriNetToDot(petriNet: PetriNet): String =
    val names = namePetriNetNodes(petriNet)
    val nodeToIndex = petriNet.nodes.zipWithIndex.toMap
    val nodeDefs = nodeToIndex.toList.map { (n, i) =>
      n match
        case p: Place      => s"$i [label=\"${names(n)}\";shape=oval]"
        case t: Transition => s"$i [label=\"${names(n)}\";shape=box]"
    }
    val edges = petriNet.edges.map { (n, m) =>
      val i1 = nodeToIndex(n)
      val i2 = nodeToIndex(m)
      s"$i1 -> $i2"
    }
    "digraph G {\n" +
      nodeDefs.mkString("\n") + "\n" +
      edges.mkString("\n") + "\n" +
      "}"

  def namePetriNetNodes(petriNet: PetriNet): Map[Node, String] =
    val trs = petriNet.transitions
    val pls = petriNet.places
    val trsMap: Map[Node, String] = trs.zipWithIndex.map { (n, i) =>
      if n.label.isEmpty then (n, s"tr$i")
      else (n, s"{${n.label}}")
    }.toMap
    val plsMap = pls.zipWithIndex.map { (n, i) =>
      if n.label.isEmpty then (n, s"pl$i")
      else (n, s"{${n.label}}")
    }.toMap
    trsMap ++ plsMap

  def petriNetToTina(petriNet: PetriNet): String =
    val map = namePetriNetNodes(petriNet)
    val sorted = map.toList.sortBy { (n, s) =>
      n match
        case t: Transition => (-1, s)
        case t: Place => (1, s)
    }
    val nodes = sorted.map { (n, s) =>
      n match
        case t: Transition =>
          val preds = petriNet.predecessors(n).map(map).mkString(" ")
          val succs = petriNet.successors(n).map(map).mkString(" ")
          s"tr $s $preds -> $succs"
        case p: Place =>
          val mark = if n eq petriNet.root then 1 else 0
          s"pl $s ($mark)"
    }
    "net mpi\n" +
      nodes.mkString("\n")
