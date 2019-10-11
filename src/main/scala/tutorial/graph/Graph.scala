package tutorial.graph

import java.net.URLEncoder
import java.nio.charset.Charset

/**
 * Represents a directed graph.
 * Use [[Graph$.empty]] from the companion object to obtain a new empty graph.
 */
sealed trait Graph {
  import Graph.Node
  /** Returns a new graph including the given edge. */
  def addEdge(from: Node, to: Node): Graph
  /** Returns a new graph including a new node with the given label and a reference to the new node. */
  def addNode(label: String): (Graph, Node)
  /** Returns an url to view the graph in a web browser */
  def show: String
}


object Graph {
  type Node = Int

  /** Creates an empty graph. */
  def empty: Graph = new GraphImpl { val edges = Set.empty; val nodeLabels = Map.empty }

  private sealed abstract class GraphImpl extends Graph { self =>
    private type Edge = (Node, Node)
    protected val nodeLabels: Map[Int, String]
    protected val edges: Set[Edge]

    def addEdge(from: Node, to: Node): Graph = {
      new GraphImpl { val edges = self.edges + ((from, to)); val nodeLabels = self.nodeLabels }
    }

    def addNode(label: String): (Graph, Node) = {
      val nextNode = if (nodeLabels.keys.isEmpty) { 0 } else { nodeLabels.keys.max + 1 }
      val nextGraph =
        new GraphImpl { val edges = self.edges; val nodeLabels = self.nodeLabels + (nextNode -> label) }
      (nextGraph, nextNode)
    }

    def show: String = {
      val dotCode =
        s"""digraph G {
           |  ${nodeLabels.iterator.map { case (node, label) => s"""$node [label = "$label"]""" } .mkString(";\n  ")}
           |  ${edges.map { case (from, to) => s"""$from -> $to"""}.mkString(";\n  ")}
           |}
           |""".stripMargin
      s"https://dreampuf.github.io/GraphvizOnline/#${URLEncoder.encode(dotCode, Charset.defaultCharset()).replace("+", "%20")}"
    }
  }


}
