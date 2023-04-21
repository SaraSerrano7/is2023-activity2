package factorial

import state.State

def multByN(n: Int): State[Int, Unit] = ???

def factorial(n: Int): Int =
  State.traverse(???)(???).run(???)._2
