package factorial

import munit.FunSuite

class FibonacciSuite extends FunSuite {

  val facts = List(1, 1, 2, 6, 24, 120, 720)

  test("factorial") {
    val results = (0 until facts.length).map(factorial)

    assertEquals(results.toList, facts)
  }
}
