package factorial

import state.State

def multByN(n: Int): State[Int, Unit] = 
  State.apply{
    // State[S, A] = S => (A, S)
    // State[Int, Unit]
    (n2: Int) =>
      if n * n2 == 0 then ((), 1) else 
      ((), n * n2)
  }

def factorial(n: Int): Int =
  State.traverse((0 to n).toList)(multByN).run(n)._2
