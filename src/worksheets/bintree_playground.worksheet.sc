//import bintree.*
//import BinaryTree.*
//import state.State
/*
import bintree.* 

val tree = Branch(Empty, 42, Empty)

tree

val tree2: BinaryTree[Int] = Branch(Branch(Empty, 1, Empty), 2, Branch(Empty, 3, Empty))
traverse(tree2)(Some.apply)
Some(tree2)
Some.apply(2)

val dice = List(1, 2, 3, 4, 5, 6)
val sums = map2ViaMatch(dice, dice)(_ + _)

val l1 = List(1, 2)
val l2 = List("a", "b")

map2ViaMatch(l1, l2)(_ -> _)
map2ViaFor(l1, l2)(_ -> _)
map2ViaFlatMap(l1, l2)(_ -> _)

val l3 = List("x", "y")

//map3ViaMatch2(l1, l2, l3)(_ -> (_, _))
map3ViaFor2(l1, l2, l3)(_ -> (_, _))
map3ViaFlatMap2(l1, l2, l3)(_ -> (_, _))
//val someTree: BinaryTree[Option[Int]] = Branch(Branch(None, Some(1), None), Some(2), Branch(None, Some(3), None))

//map3ViaFlatMap2(l1, l2, l3)(Branch(_, _, _))

def mkTree(l: Int, v: Int, r: Int) = Branch(Branch(Empty, l, Empty), v, Branch(Empty, r, Empty))
List((1, 10, 100), (1, 10, 200), (1, 20, 100), (1, 20, 200),
    (2, 10, 100), (2, 10, 200), (2, 20, 100), (2, 20, 200)).map(mkTree.tupled)

def sameAndDouble(n: Int) = List(n, n * 2)
val tree3 = mkTree(1, 10, 100)
traverse(tree3)(sameAndDouble)

val s1 = State[Int, Int](s => (s * 2, s + 1))
val s2 = State[Int, Int](s => (s + 2, s + 2))
val s3 = State[Int, Int](s => (s / 2, s - 3))
val combined = map3(s1, s2, s3)

*/