package tutorial

import tutorial.graph.Graph
import tutorial.graph.Graph.Node

/** The builder pattern. */
object Lecture02 extends App {
  /* The code of [[Lecture01]] does not use any domain-specific abstractions.
   * It operates entirely in the target language.
   * We will now improve on this using the builder pattern.
   */

  /** An interface for building boolean circuits. */
  trait CircuitBuilder[Result, Pin] {
    /** Adds a new input or output pin. */
    def addIOPin(name: String): (CircuitBuilder[Result, Pin], Pin)
    /** Adds a new nor gate, returning its two input and one output pin */
    def addNorGate(): (CircuitBuilder[Result, Pin], ((Pin, Pin), Pin))
    /** Connects two given pins. */
    def connect(fromPin: Pin, toPin: Pin): CircuitBuilder[Result, Pin]

    /** Builds the final result in the target language. */
    def build: Result
  }

  class GraphBasedCircuitBuilder(result: Graph = Graph.empty) extends CircuitBuilder[Graph, Node] {
    def addIOPin(name: String): (GraphBasedCircuitBuilder, Node) = {
      val (nextGraph, pin) = build.addNode(name)
      (new GraphBasedCircuitBuilder(nextGraph), pin)
    }


    def addNorGate(): (CircuitBuilder[Graph, Node], ((Node, Node), Node)) = {
      /* TODO: implement adding a nor gate. */
      ???
    }

    def connect(fromPin: Node, toPin: Node): GraphBasedCircuitBuilder = {
      /* TODO: implement adding a connection. */
      ???
    }

    def build: Graph = result
  }

  /* Now we rebuild the NOR flip-flop. */
  val emptyCircuit = new GraphBasedCircuitBuilder()
  val (withR, r) = emptyCircuit.addIOPin("R")
  val (withS, s) = withR.addIOPin("S")
  val (withQ, q) = withS.addIOPin("Q")
  val (withNQ, nq) = withQ.addIOPin("nQ")
  val (withNOR1, ((nor1Input1, nor1Input2), nor1Output)) = withNQ.addNorGate()
  val (withNOR2, ((nor2Input1, nor2Input2), nor2Output)) = withNOR1.addNorGate()

  val result =
    withNOR2
    .connect(r, nor1Input1)
    .connect(nor2Output, nor1Input2)
    .connect(s, nor2Input1)
    .connect(nor1Output, nor1Input2)
    .connect(nor1Output, q)
    .connect(nor2Output, nq)
    .build

  println(result.show)
}
