import bintree.*
import BinaryTree.*

import sum.*
import state.State


val action = accumulateAction(2).run(3)
((), 5)

val ints = List(5, 2, 4, -2, 1, -8, 0)

State.traverse(ints)(accumulateAction(_)).run(0)

val tree = Branch(Branch(Empty, 2, Empty), 1, Branch(Empty, 3, Empty))
//BinaryTree.traverse(tree)(tree)(accumulateAction(_))