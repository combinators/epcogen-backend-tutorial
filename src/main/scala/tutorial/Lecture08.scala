package tutorial

import tutorial.graph.Graph
import tutorial.graph.Graph.Node

/* Different ways to execute the same plan - a failed attempt. */
object Lecture08 extends App {
  /* In the previous lectures, we always compiled out plan to a graph.
   * But what if we want a text format instead?
   * In the current design, the run method is part of the plan and hard-wires the state type:
   * ```
   * sealed trait Plan[R] {
   *   def run(state: Graph): (Graph, R)
   * }
   * ```
   * At the moment, just like in traditional OO programming, adding an operation to execute plans requires
   * patching up all classes in our infrastructure.
   * As a first idea, we might try to convert this into a functional style, which allows to easily add methods.
   * This won't be the final solution, but it is interesting nevertheless and illustrates some problems with tutorials
   * you can find on "free monads", which are what we have done so far.
   * To create the compiler function, we return to the plans from Lecture05, which do not have a run-method.
   */
  import Lecture05.{Plan, ExistingResult, Sequence, PerformTask, Task, AddIOPin, AddNorGate, Connect}

  /* Now we write external run methods for tasks and a plan. */
  def runGraphTask[R](graph: Graph, task: Task[R]): (Graph, R) = {
    task match {
      case Connect(from: Node, to: Node) =>
        (graph.addEdge(from, to), ().asInstanceOf[R])
      case AddNorGate() =>
        val (nextGraph, gate) = graph.addNode("NOR")
        (nextGraph, ((gate, gate), gate).asInstanceOf[R])
      case AddIOPin(name) =>
        /* TODO: implement this case */
        ???
    }
  }

  /* As you can see in the example above, we already run into trouble:
   * What if runGraphTask gets passed a Connect(x, y) where x and y are some other type than Node?
   * We cannot fix this by restricting the type R which is Unit for the command Connect, so we would need a new
   * type parameter on Tasks.
   * This might be possible, but it does not solve the second issue:
   * Scala is not able to handle the dependent pattern matching, which infers that ((Node, Node), Node) is the
   * return type R when adding a Nor gate and Unit is the return type when connecting two pins.
   * This requires us to use `.asInstanceOf[R]`, which is a dangerous idea (e.g. our example would break if R is not
   * related to the types used for graphs).
   *
   * Finally, we also performed the classical expression problem OO vs functional trade with the devil:
   * we can now add a second `run` function, but adding e.g. a NAND-gate will require us to change all the compilers.
   *
   * For now, lets try to proceed anyways.
   */

  def runGraphPlan[R](graph: Graph, plan: Plan[R]): (Graph, R) = {
    plan match {
      case ExistingResult(result) => (graph, result)
      case PerformTask(task) =>
        /* TODO: implement this case using runGraphTask. */
        ???
      case Sequence(firstPlan, future) =>
        val (updatedGraph, intermediateResult : AnyRef) = runGraphPlan(graph, firstPlan)
        runGraphPlan(updatedGraph, future.asInstanceOf[Function[intermediateResult.type, Plan[R]]](intermediateResult))
    }
  }
  /* We can see that the type cast problems become horrible in this solution, because Sequence includes
   * what type-theorists call a cut-type - the intermediate type R1 is not visible to the outside.
   * Path dependent types could fix this, but we will not go there.
   */

  val (resultGraph, _) = runGraphPlan(Graph.empty, Lecture05.flipFlopPlan[Node])
  println(resultGraph.show)



}
