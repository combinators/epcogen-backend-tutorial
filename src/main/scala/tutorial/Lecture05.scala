package tutorial

import tutorial.graph.Graph

/* Planing the future. */
object Lecture05 extends App {
  /* In the last lecture we explicitly managed the future of our computations using CPS.
   * This allowed us to delimit the lifetime of an intermediate computation result using functions.
   * However, it forces us to constantly worry about managing the future.
   * Lets return to the boolean circuits, but simplify things and - just as in real life - create a plan.
   */

  /* We first need a base class for our plan, which has the (intermediate) result R */
  sealed trait Plan[R]

  /* The easiest kind of plan is having somebody else do the work. */
  final case class ExistingResult[R](result: R) extends Plan[R]

  /* We also need to denote some task which results in something of type R */
  sealed trait Task[R]
  /* Such as inserting a Pin */
  final case class AddIOPin[Pin](name: String) extends Task[Pin]
  /* Inserting a gate */
  final case class AddNorGate[Pin]() extends Task[((Pin, Pin), Pin)]
  /* Or connecting two pins, which has no result (Unit) */
  final case class Connect[Pin](from: Pin, to: Pin) extends Task[Unit]

  /* Now let us add a Task to the plan */
  final case class PerformTask[R](task: Task[R]) extends Plan[R]

  /* So far, plans are fairly stupid and just consist of one result or task.
   * For something more interesting, we want to be able to sequentially put together plans, using the result of
   * the previous plan for constructing the next plan.
   * The sequencing is done just like in CPS, where we ask for a future function.
   */
  final case class Sequence[R1, R2](firstPlan: Plan[R1], future: R1 => Plan[R2]) extends Plan[R2]

  /* We may now create plans, even though we have no idea how to execute them yet. */
  def flipFlopPlan[Pin]: Plan[Unit] = {
    Sequence(
      PerformTask(AddIOPin[Pin]("R")),
      (r: Pin) => Sequence(
        PerformTask(AddIOPin[Pin]("S")),
        (s: Pin) => Sequence(
          PerformTask(AddIOPin[Pin]("Q")),
          (q: Pin) => Sequence(
            /* TODO: Add the nQ pin */
            PerformTask(AddNorGate[Pin]()),
            (nor1Pins: ((Pin, Pin), Pin)) => {
              val ((nor1Input1, nor1Input2), nor1Output) = nor1Pins
              Sequence(
                /* TODO: Add the other NOR gate */
                PerformTask(Connect(r, nor1Input1)),
                (_ : Unit) => Sequence(
                  PerformTask(Connect(nor1Output, q)),
                  (_ : Unit) =>
                    /* TODO: Wire up the rest of the nor flip-flop */
                    ExistingResult(())
                )
              )
            }
          )
        )
      )
    )
  }
}
