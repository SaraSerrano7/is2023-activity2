import hondt.LazyList.*
import hondt.* 

val infinite = createInfinite(1)

def createInfinite(n: Int): LazyList[Int] =
    cons(n, createInfinite(n+1))

val threeList = infinite.take(3).toList

val inf = LazyList.from(1).take(5).toList.head

val l1 = LazyList(10.0, 8.0, 5.0).map(("a", _))
val l2 = LazyList(9.0, 7.0, 6.0).map(("b", _))
val merged = merge(l1, l2).toList
val mergedOk = List(("a", 10.0), ("b", 9.0), ("a", 8.0), ("b", 7.0), ("b", 6.0), ("a", 5.0))

//----------------------

def quotient(numerator: Int, denominator: Int): Double =
    println(s"computing quotient $numerator / $denominator")
    numerator.toDouble / denominator

def partyQuotients(party: String, numVotes: Int): LazyList[(String, Double)] =
    val infiniteDenominators = LazyList.from(1)
    val infiniteQuotients = infiniteDenominators.map(quotient(numVotes, _))
    infiniteQuotients.map((party, _))

def merge(
      left: LazyList[(String, Double)],
      right: LazyList[(String, Double)]
  ): LazyList[(String, Double)] =
    (left, right) match
      case (Empty, _) => right
      case (_, Empty) => left
      case (Cons(h1, t1), Cons(h2, t2)) => 
        val firstLeft = h1()
        val firstRight = h2()
        if firstLeft._2 > firstRight._2 then cons(firstLeft, merge(t1(), right)) 
                                        else cons(firstRight, merge(left, t2()))


val n: Int = 15
val votes: Map[String, Int] =
    Map("JxCat" -> 45_029,
    "ERC" -> 42_670,
    "PSC" -> 24_115,
    "CUP-G" -> 11_871,
    "VOX" -> 8_876,
    "PDeCAT" -> 7_365,
    "PP" -> 5_189,
    "Cs" -> 5_175)

val quotients = 
      votes.iterator.map(partyQuotients(_, _)) // Iterator[LazyList[(String, Double)]]

val orderedQuotients: LazyList[(String, Double)] =
      quotients.fold(Empty)(merge)

val selectedQuotients: LazyList[(String, Double)] =
      orderedQuotients.take(n)
      
val result = selectedQuotients.toList//: Map[String, Int] =

val result2 = result.groupMapReduce((party, _) => party)(_ => 1)(_ + _)

val resultOk =
      Map("CUP-G" -> 1,
        "JxCat" -> 5,
        "VOX" -> 1,
        "ERC" -> 5,
        "PSC" -> 3)