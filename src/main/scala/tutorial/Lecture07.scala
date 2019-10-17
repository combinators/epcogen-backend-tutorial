package tutorial

import tutorial.Lecture06.Plan
import tutorial.graph.Graph
import tutorial.graph.Graph.Node

/* Improving notation. */
object Lecture07 extends App {
  /* In all of the prior lectures the notation to create a flip-flop was noisy:
   * - with plain graphs or the builder, state hat to be carried around manually
   * - with plans it looks like a bad lisp program with too many parentheses
   *
   * Luckily, Scala has syntactic sugar to fix this (this is shared by most other functional languages).
   * We will need two methods in our Plan type to enable the syntactic simplicfication: `flatMap` and `map`.
   * Method flatMap is just like Sequence, it schedules a future plan after this plan.
   * The same is done by map, just that instead of scheduling a future plan, we schedule a function to be applied to the
   * current intermediate result.
   */
  sealed trait Plan[R] {
    /* Takes a current graph, executes this plan and returns the updated graph and a result.  */
    def run(state: Graph): (Graph, R)

    /* Schedules the next plan, which is based on the current result. */
    def flatMap[NR](future: R => Plan[NR]): Plan[NR] =
      Sequence(this, future)

    /* Schedules a function to be applied to the current result. */
    def map[NR](function: R => NR): Plan[NR] =
      flatMap(result => ExistingResult(function(result)))
  }

  /* We proceed just like in Lecture06 */

  final case class ExistingResult[R](result: R) extends Plan[R] {
    def run(currentGraph: Graph): (Graph, R) = (currentGraph, result)
  }
  final case class Sequence[R1, R2](firstPlan: Plan[R1], future: R1 => Plan[R2]) extends Plan[R2] {
    def run(currentGraph: Graph): (Graph, R2) = {
      val (nextGraph, nextResult) = firstPlan.run(currentGraph)
      future(nextResult).run(nextGraph)
    }
  }
  sealed trait Task[R] {
    def run(currentGraph: Graph): (Graph, R)
  }
  final case class AddNorGate() extends Task[((Node, Node), Node)] {
    def run(currentGraph: Graph): (Graph, ((Node, Node), Node)) = {
      val (nextGraph, gate) = currentGraph.addNode("NOR")
      (nextGraph, ((gate, gate), gate))
    }
  }
  final case class AddIOPin(name: String) extends Task[Node] {
    def run(currentGraph: Graph): (Graph, Node) = {
      currentGraph.addNode(name)
    }
  }
  final case class Connect(from: Node, to: Node) extends Task[Unit] {
    def run(currentGraph: Graph): (Graph, Unit) = {
      (currentGraph.addEdge(from, to), ())
    }
  }
  final case class PerformTask[R](task: Task[R]) extends Plan[R] {
    def run(currentGraph: Graph): (Graph, R) = task.run(currentGraph)
  }

  /* Now we can first build our plan in a more object oriented style */
  def flipFlopOO: Plan[Unit] = {
    PerformTask(AddIOPin("R")).flatMap { r =>
      PerformTask(AddIOPin("S")).flatMap { s =>
        PerformTask(AddIOPin("Q")).flatMap { q =>
          /* TODO: Add the nQ pin */
            PerformTask(AddNorGate()).flatMap { nor1Pins =>
              val ((nor1Input1, nor1Input2), nor1Output) = nor1Pins
              /* TODO: Add the other NOR gate */
                PerformTask(Connect(r, nor1Input1)).flatMap { _ =>
                  PerformTask(Connect(nor1Output, q)).flatMap { _ =>
                    /* TODO: Wire up the rest of the nor flip-flop */
                    ExistingResult(())
                  }
                }
            }

        }
      }
    }
  }
  val (resultGraph1, ()) = flipFlopOO.run(Graph.empty)
  println(resultGraph1.show)

  /* This is better, but still no cigar.
   * Here comes the syntactic sugar:
   * When you write a for-loop
   * ```
   * for {
   *   x <- o1
   *   (y1, y2) <- o2(x)
   *   z <- o3(y2)
   * } yield f(z)
   * ```
   * Scala will translate it as follows:
   * o1.flatMap(x => o2(x).flatMap { _ match { case (y1, y2) => o3(y2).map(z => f(z)) }})
   *
   * Note that this is equivalent to writing:
   * o1.flatMap(x => o2(x).flatMap { foo => foo match { case (y1, y2) => o3(y2).map(z => f(z)) }})
   * for a fresh variable name `foo`.
   *
   * That is:
   * - Method `flatMap` is called for things on the right of an arrow.
   * - The future passed to flat map is a function, which
   *   + has the variable on the left of the arrow as its parameter
   *   + or performs pattern matching if the left of the arrow is a pattern
   *   + returns the rest of the translated for-loop
   * - The final arrow in the loop is translated by calling .map on the right hand side
   * - The function passed to map
   *   + has the variable on the left of the arrow as its parameter
   *   + or performs pattern matching if the left of the arrow is a pattern
   *   + returns the expression after the yield as its result
   *
   * Lets try it with a simple NOR-Gate:
   *
   *      _____
   * x ->|     |
   *     | NOR | -> out
   * y ->|_____|
   *
   */
  def easyNorFor: Plan[Node] =
    for {
      x <- PerformTask(AddIOPin("x"))
      y <- PerformTask(AddIOPin("y"))
      out <- PerformTask(AddIOPin("out"))
      ((norIn1, norIn2), norOut) <- PerformTask(AddNorGate())
      con1 <- PerformTask(Connect(x, norIn1))
      con2 <- PerformTask(Connect(y, norIn2))
      con3 <- PerformTask(Connect(norOut, out))
    } yield out

  val (resultGraph2, out2) = easyNorFor.run(Graph.empty)
  println(resultGraph2.show)

  def easyNorOO: Plan[Node] = {
    /* TODO: Translate the NOR-Gate above to explicit OO-style */
    ???
  }
  /* TODO: Show the OO-style gate to verify your translation. */

  /* Of course, the same translation is possible in reverse, if you like thinking the other way around: */
  def norNorOO: Plan[Node] = {
    PerformTask(AddIOPin("in1")).flatMap { in1 =>
      PerformTask(AddIOPin("in2")).flatMap { in2 =>
        PerformTask(AddIOPin("in3")).flatMap { in3 =>
          PerformTask(AddNorGate()).flatMap {
            _ match {
              case ((nor1In1, nor1In2), nor1Out) =>
                PerformTask(AddNorGate()).flatMap {
                  _ match {
                    case ((nor2In1, nor2In2), nor2Out) =>
                      PerformTask(Connect(in1, nor1In1)).flatMap { con1 =>
                        PerformTask(Connect(in2, nor1In2)).flatMap { con2 =>
                          PerformTask(Connect(in3, nor2In2)).flatMap { con3 =>
                            PerformTask(Connect(nor1Out, nor2In1)).flatMap { con4 =>
                              PerformTask(AddIOPin("result")).flatMap { result =>
                                PerformTask(Connect(nor2Out, result)).map { con5 =>
                                  result
                                }
                              }
                            }
                          }
                        }
                      }
                  }
                }
            }
          }
        }
      }
    }
  }
  val (resultGraph4, out4) = norNorOO.run(Graph.empty)
  println(resultGraph4.show)

  /* TODO: Translate norNorOO to a for-loop based construction. */
  def norNorFor: Plan[Node] = {
    ???
  }
  /* TODO: Show the OO-style gate to verify your translation. */

  /* In the example above, we did not care about some of the intermediate results, because drawing a connection
   * between two pins returns unit.
   * IntelliJ (and other IDEs) detect this and highlight that the conX variables are not used.
   * This situation is so common, that there is a convention for it.
   * Whenever we do not use a variable, we name it underscore `_`.
   * The for-loop example usually would have been written:
   */
  def easyNorForUnderscore: Plan[Node] =
    for {
      x <- PerformTask(AddIOPin("x"))
      y <- PerformTask(AddIOPin("y"))
      out <- PerformTask(AddIOPin("out"))
      norPins <- PerformTask(AddNorGate())
      _ <- PerformTask(Connect(x, norPins._1._1))
      _ <- PerformTask(Connect(y, norPins._1._2))
      _ <- PerformTask(Connect(norPins._2, out))
    } yield out

  /* So the final two calls in here translate to
   * ```
   * PerformTask(Connect(y, norPins._1._2)).flatMap { _ =>
   *   PerformTask(Connect(norPins._2, out)).map { _ =>
   *     out
   *   }
   * }
   * ```
   */

  /* Finally we can build our flip-flop with a for loop */
  def flipFlopFor: Plan[Unit] = {
    for {
      r <- PerformTask(AddIOPin("R"))
      /* TODO: build the rest of the flip-flop */
    } yield ()
  }
  val (resultGraph5, _) = flipFlopFor.run(Graph.empty)
  println(resultGraph5.show)

  /* As a final remark it should be mentioned, that the mechanism as described above is the simplified version of
   * for-loops provided by the Scala compiler plug-in "better-monadic-for", which is enabled in the build.sbt file.
   * The original rules of Scala are a bit more complicated and the better monadic for style will replace
   * them in Scala 3.
   * For more details, visit the plug-in homepage: https://github.com/oleg-py/better-monadic-for
   */
}
