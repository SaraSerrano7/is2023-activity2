package fibonacci

def fibonacciIter(n: Int): Int =
  var i = 0
  var a = 0
  var b = 1
  while (i < n) {
    val tmp = a
    a = b
    b = tmp + b
    i = i + 1
  }
  a

import state.State
import State.*

def fibonacci(n: Int): Int =
  case class FiboState(i: Int, a: Int, b: Int)
  lazy val fibonacciAction: State[FiboState, Int] = // Int => (Int, FiboState)
    get[FiboState].flatMap{
      case FiboState(i, a, b) => 
        if (i < n) then
          set(FiboState(i + 1, b, a + b)).flatMap{
            _ => fibonacciAction
          }
        else unit(a)
    }
  fibonacciAction.run(FiboState(0, 0, 1))._1
