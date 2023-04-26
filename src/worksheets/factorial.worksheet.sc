import factorial.* 

val facts = List(1, 1, 2, 6, 24, 120, 720)
(0 until facts.length).toList
multByN(5).run(2)

val state1 = 1

val results = (0 until facts.length).map(factorial).toList

val fact = factorial(2)