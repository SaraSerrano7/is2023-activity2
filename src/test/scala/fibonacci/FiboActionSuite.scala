package fibonacci

import munit.FunSuite

class FiboActionSuite extends FunSuite {

  val fibos = List(0, 1, 1, 2, 3, 5, 8, 13, 21)

  test("fiboIter") {
    val results = (0 until fibos.length).map(fibonacciIter)

    assertEquals(results.toList, fibos)
  }

  test("fibonacci") {
    val results = (0 until fibos.length).map(fibonacci)

    assertEquals(results.toList, fibos)
  }

}
