package tutorial

/** An improved version of the builder pattern. */
object Lecture05 {
  /* We now try to fix the issue identified in Lecture03.
   * For this, we implement an assistant.
   * When meeting our assistant, he will write down our commands, and execute them later.
   * He will remember the result of the last command, and when we pass the next command, he will allow us to refer to
   * that result.
   * It is important to ensure, that this reference to the last result only exists within a scope, where the last result
   * is known. Otherwise we end up with out-of-scope pins again.
   */
  trait Assistant[T] {

  }


}
