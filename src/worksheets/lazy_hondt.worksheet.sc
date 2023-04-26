import hondt.LazyList.cons
import hondt.* 

val infinite = createInfinite(1)

def createInfinite(n: Int): LazyList[Int] =
    cons(n, createInfinite(n+1))

val threeList = infinite.take(3).toList

val inf = LazyList.from(1).take(5).toList.head