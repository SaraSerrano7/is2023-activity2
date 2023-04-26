package sum

import bintree.BinaryTree
import state.State

// Define an action which represents the accumulation of the
// integer passed as parameter (n) into the state (which is
// also an Int). The returned value of the action is Unit (its
// only effect is changing the value of the current state).

// You can implement it various ways, for example:
// - creating the action via State.apply passing an Int => (Unit, Int)
// - using map/flatMap and the State.get / State.set actions
//   - and the combination of map/flatMap can be expressed via for-comprehension
// - using the State.modify and a function Int => Int

def accumulateAction(n: Int): State[Int, Unit] = //S => ((), Int)
  State.apply(n2 => ((), n + n2))

def accumulateActionViaFor(n: Int): State[Int, Unit] = //S => ((), Int)
    for {
      s <- State.get // S => (S, S: Int)
      s2 <- State.set(s + n) // _ => ((), S: Int)
    } yield ()

def accumulateActionViaModify(n: Int): State[Int, Unit] = //S => ((), Int)
  State.modify(n2 => n + n2)

// Implement a method that sums the elements of a list (which are integers)
// - use the accumulateAction
// - use State.traverse over the list

//State[S, +A] = S => (A, S)
def sumList(ints: List[Int]): Int = 
  val result: State[Int, List[Unit]] = State.traverse(ints)(accumulateAction(_))
  val (_, s) = result.run(0)
  s

// Using the same ideas as before, implement a method that sums the elements of a tree
// Use the implementation of BinaryTree.traverse that you have defined
def sumTree(bt: BinaryTree[Int]): Int =
  val result = BinaryTree.traverse(bt)(accumulateAction(_))
  val (_, s: Int) = result.run(0)
  s
