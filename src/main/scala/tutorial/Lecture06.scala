package tutorial

import tutorial.graph.Graph
import tutorial.graph.Graph.Node

/* Executing plans. */
object Lecture06 extends App {
  /* To make our plans from Lecture05 executable, we endow them with a `run` method.
   * For now we restrict our selves to graphs. */
  sealed trait Plan[R] {
    /* Takes a current graph, executes this plan and returns the updated graph and a result.  */
    def run(state: Graph): (Graph, R)
  }

  final case class ExistingResult[R](result: R) extends Plan[R] {
    /* We just return the result, without touching the state */
    def run(currentGraph: Graph): (Graph, R) = (currentGraph, result)
  }

  final case class Sequence[R1, R2](firstPlan: Plan[R1], future: R1 => Plan[R2]) extends Plan[R2] {
    /* For sequences, we run the first plan, then compute the future plan and execute it. */
    def run(currentGraph: Graph): (Graph, R2) = {
      val (nextGraph, nextResult) = firstPlan.run(currentGraph)
      /* TODO: implement the rest of this function */
      ???
    }
  }

  /* Observe how so far, the Graph could have been any type of state - e.g. the AST of a VHDL Program.
   * This changes when we consider actual commands.
   * For now we need to force that the type of Pins is always Node, because we do not know what else to do with commands.
   * We will recover a more general solution later. */
  sealed trait Task[R] {
    /* Works as for Plan */
    def run(currentGraph: Graph): (Graph, R)
  }

  /* Inserting a gate */
  final case class AddNorGate() extends Task[((Node, Node), Node)] {
    def run(currentGraph: Graph): (Graph, ((Node, Node), Node)) = {
      val (nextGraph, gate) = currentGraph.addNode("NOR")
      (nextGraph, ((gate, gate), gate))
    }
  }

  final case class AddIOPin(name: String) extends Task[Node] {
    def run(currentGraph: Graph): (Graph, Node) = {
      /* TODO: implement this method */
      ???
    }
  }

  /* Or connecting two pins, which has no result (Unit) */
  final case class Connect(from: Node, to: Node) extends Task[Unit] {
    def run(currentGraph: Graph): (Graph, Unit) = {
      /* TODO: implement this method */
      ???
    }
  }

  /* Performing a task is just dispatching to its run method now. */
  final case class PerformTask[R](task: Task[R]) extends Plan[R] {
    def run(currentGraph: Graph): (Graph, R) = task.run(currentGraph)
  }

  /* Time to recreate the flip-flop. */
  def flipFlopPlan: Plan[Unit] = {
    Sequence(
      PerformTask(AddIOPin("R")),
      (r: Node) => Sequence(
        PerformTask(AddIOPin("S")),
        (s: Node) => Sequence(
          PerformTask(AddIOPin("Q")),
          (q: Node) => Sequence(
            PerformTask(AddIOPin("nQ")),
            (nq: Node) => Sequence(
              PerformTask(AddNorGate()),
              (nor1Pins: ((Node, Node), Node)) => {
                val ((nor1Input1, nor1Input2), nor1Output) = nor1Pins
                Sequence(
                  PerformTask(AddNorGate()),
                  (nor2Pins: ((Node, Node), Node)) => {
                    val ((nor2Input1, nor2Input2), nor2Output) = nor2Pins
                    Sequence(
                      PerformTask(Connect(r, nor1Input1)),
                      (_: Unit) => Sequence(
                        PerformTask(Connect(nor1Output, q)),
                        (_: Unit) => Sequence(
                          PerformTask(Connect(nor2Output, nor1Input2)),
                          (_: Unit) => Sequence(
                            PerformTask(Connect(nor1Output, nor2Input1)),
                            (_: Unit) => Sequence(
                              PerformTask(Connect(s, nor2Input2)),
                              (_: Unit) => PerformTask(Connect(nor2Output, nq))
                            )
                          )
                        )
                      )
                    )
                  }
                )
              }
            )
          )
        )
      )
    )
  }
  /* Our run method returns the final graph and Unit, hence we do: */
  val (resultGraph, ()) = flipFlopPlan.run(Graph.empty)
  println(resultGraph.show)
}
