package tutorial

import graph.Graph

/** Boolean circuits as Graphs. */
object Lecture01 extends App {

  /* We are going to write a code generator for boolean circuits.
   *
   * The first target language will be a graph-based representation.
   * Check [[graph.Graph]] for the representation of graphs.
   *
   * The first task is to construct a NOR flip-flop.
   * Its graph has two input nodes, R and S, and two output nodes Q and nQ.
   * Node R is connected to a node labeled by NOR.
   * The output of this NOR node is connected to Q and the input of another NOR node.
   * Node S is connected to to the input of the second NOR node.
   * The output of the second NOR node is connected to nQ and the input of the first NOR node.
   */

  val emptyFlipFlop = Graph.empty
  val (withR, r) = emptyFlipFlop.addNode("R")
  val (withS, s) = withR.addNode("S")
  val (withNOR1, nor1) = withS.addNode("NOR")
  val (withNOR2, nor2) = withNOR1.addNode("NOR")
  val connectedRNOR1 = withNOR2.addEdge(r, nor1)
  val connectedSNOR2 = connectedRNOR1.addEdge(s, nor2)

  println(s"Graph to complete: ${connectedSNOR2.show}")

  /* TODO: Complete the graph as described above and show it. */

}
