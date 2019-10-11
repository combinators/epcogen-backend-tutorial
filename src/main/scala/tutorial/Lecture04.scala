package tutorial

/** CPS: Delimiting the scope of results. */
object Lecture04 extends App {
  /* One of the issues in Lecture03 was that pin S was not in scope, because the builder state withNOR2, in which
   * connections are wired up, was not build upon a state including S.
   * We can address this problem by enforcing valid scopes with the scopes of functions in our implementation language.
   * To do this, our program needs to be written in Continuation Passing Style (CPS), which is a standard
   * programming trick in compilers.
   * It also has a deep logical meaning, and it is the underpinning of exceptions.
   * Here, we will focus on delimiting scopes, but feel free to check
   * https://www-seal.cs.tu-dortmund.de/seal/downloads/teaching/lmse1819/V17.pdf
   * if you are interested on logic and more (skip the first 2 slides, the rest is in english).
   *
   * In the boolean circuit example, we had a result value (pin) and a state (the builder).
   * For this lecture, we want to keep things simple and just worry about a result value. */

  /* Let us build a very small program to add some numbers. */
  val x1 = 5
  val x2 = x1 * x1
  val x3 = x1 + x2
  val x4 = x3 - 5
  println(s"Got: $x4 Expected: 25")

  /* So far so good, but what about: */
  val y1 = 5
  val y2 = y1 * y1
  val z1 = 4
  val z2 = z1 + y3
  val y3 = y2 - 1
  val z3 = z2 - 3
  println(s"Got: $z3 Expected: 25")

  /* IntelliJ helps us to spot the mistake: y3 is used before it is initialized.
   * It therefore is 0 and we get z3 == 1 instead of z3 == 25.
   * This is easy to fix, by moving the declaration of y3, but we have seen that things are not always as easy.
   *
   * Let us rearrange our computation a bit.
   * Now, instead of just returning the result:
   * - we take a function, which is /the future/
   * - we perform our computation and pass the result to the future
   * - if we depend on another computation, we call it, passing our selves as its future,
   *   because it has to be performed in our past.
   */
  def withY1(future: Int => Int): Int = {
    future(5)
  }
  def withY2(future: Int => Int): Int = {
    withY1(y1 => {
      future(y1 * y1)
    })
  }
  def withZ1(future: Int => Int): Int = {
    future(4)
  }
  def withZ2(future: Int => Int): Int = {
    withZ1(z1 => {
      withY3(y3 => {
        future(z1 + y3)
      })
    })
  }
  def withY3(future: Int => Int): Int = {
    withY2(y2 => {
      future(y2 - 1)
    })
  }
  def withZ3(future: Int => Int): Int = {
    withZ2(z2 => {
      future(z2 - 3)
    })
  }

  /* The final step is to pass a future returning the current result into withZ3. */
  val z3cps = withZ3(z3 => {
    z3
  })
  println(s"Got: $z3cps Expected: 25")

  /* TODO: print some intermediate results. */

  /* TODO: set a breakpoint on the line of z3cps and observe the order in which things are executed by stepping
      through the code. */

  /* TODO: transform the first computation into CPS. */

  /* The code above forces futures to always return an Int.
   * If others need our code, this would be bad.
   * Lucily it can be easily fixed using a generic parameter: */
  def withY1Generic[R](future: Int => R): R = {
    future(5)
  }
  def withY2Generic[R](future: Int => R): R = {
    withY1Generic(y1 => {
      future(y1 * y1)
    })
  }
  val y2GenericCPS: String = withY2Generic[String](y2 => s"Got $y2 Expected: 25")
  println(y2GenericCPS)

  /* TODO: implement withY3Generic and print the resulting computation. */

  /* We can now wrap our types to avoid writing to much: */
  type Future[A, R] = A => R
  type CPSValue[A, R] = Future[A, R] => R

  /* Previously, we implemented things with fixed values, but we can also build combinators which take arbitrary CPS
   * style values as input. */
  def add[R](withX: CPSValue[Int, R], withY: CPSValue[Int, R]): CPSValue[Int, R] = future => {
    withX(x => {
      withY(y => {
        future(x + y)
      })
    })
  }

  def withAddedY[R] = add[R](withY2Generic, withY1Generic)
  val withAddedYResult = withAddedY(result => s"Got: $result Expected: 30")
  println(withAddedYResult)

  /* TODO: implement multiplication. */

  /* Here is a final excursion not so relevant to the rest of the tutorial.
   * With control over the future, we may implement our own exception handling mechanism - just by skipping the
   * normal future if our plan fails. */
  def div[R](
    withNominator: CPSValue[Int, R],
    withDenominator: CPSValue[Int, R],
    onByZero: R): CPSValue[Int, R] = future => {
    withNominator(nominator => {
      withDenominator(denominator => {
        if (denominator == 0) {
          onByZero // Abort future computations and just return
        } else {
          future(nominator / denominator) // continue as usual
        }
      })
    })
  }

  def withZero[R]: CPSValue[Int, R] = future => future(0)

  val exceptionExample: CPSValue[Int, String] = {
    withZero
    /* TODO: replace the above withZero by (y1 / 0) + y2 using withY1Generic, withZero, withY2Generic, add, div and a message for errors. */
  }

  val exceptionResult = exceptionExample(result => s"Got: ${result} Expected: This never happens.")
  println(exceptionResult)
}
