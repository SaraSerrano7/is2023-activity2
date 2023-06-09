package sum

import munit.FunSuite

class SumActionSuite extends FunSuite {

  test("sumAction") {
    val action1 = accumulateAction(2)
    assertEquals(action1.run(3), ((), 5))
    val action2 = accumulateActionViaFor(5)
    assertEquals(action2.run(0), ((), 5))
    val action3 = accumulateActionViaModify(1)
    assertEquals(action3.run(1), ((), 2))
  }

  test("sumList") {
    val ints = List(5, 2, 4, -2, 1, -8, 0)
    assertEquals(sumList(ints), ints.sum)
  }

  test("sumTree") {
    import bintree.BinaryTree.*
    val tree = Branch(Branch(Empty, 2, Empty), 1, Branch(Empty, 3, Empty))
    assertEquals(sumTree(tree), 6)
  }
}
