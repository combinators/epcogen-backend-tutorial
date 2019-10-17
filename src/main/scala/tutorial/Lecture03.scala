package tutorial

import tutorial.Lecture02.GraphBasedCircuitBuilder

/** Problems with the builder pattern. */
object Lecture03 {

  /* Let us recapture the code to build the NOR flip-flop from the last lecture */
  val emptyCircuit = new GraphBasedCircuitBuilder()
  val (withR, r) = emptyCircuit.addIOPin("R")
  val (withS, s) = withR.addIOPin("S")
  val (withQ, q) = withR.addIOPin("Q")
  val (withNQ, nq) = withQ.addIOPin("nQ")
  val (withNOR1, ((nor1Input1, nor1Input2), nor1Output)) = withQ.addNorGate()
  val (withNOR2, ((nor2Input1, nor2Input2), nor2Output)) = withNOR1.addNorGate()

  val result =
    withNOR2
      .connect(r, nor1Input1)
      .connect(nor2Output, nor1Input2)
      .connect(s, nor2Input1)
      .connect(nor1Output, nor2Input2)
      .connect(nor1Output, q)
      .connect(nor2Output, nq)
      .build

  println(result.show)

  /* Examine the result.
   * The code above was not copied correctly.
   * TODO: Find and fix the error without revisiting Lecture02.
   * Explain the problem, which makes the API prone to this kind of errors.
   */
}
