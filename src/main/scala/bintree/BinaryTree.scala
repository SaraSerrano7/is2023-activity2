package bintree

enum BinaryTree[+A]:
  case Empty
  case Branch(left: BinaryTree[A], value: A, right: BinaryTree[A])

  def fold[B](z: B)(f: (B, A, B) => B): B =
    this match
      case Empty           => z
      case Branch(l, v, r) => f(l.fold(z)(f), v, r.fold(z)(f))

  def map[B](f: A => B): BinaryTree[B] =
    fold(Empty: BinaryTree[B])((l, v, r) => Branch(l, f(v), r))

object BinaryTree:

  // Option

  // Define map3 for Option which combines the contents of three Options with a funcion.
  // If any of the Options is None the resulting Option is None
  // Try to do it using high-level combinators such a map/flatMap or for-comprehensions

  def map3ViaMatch[A, B, C, D](oa: Option[A], ob: Option[B], oc: Option[C])(
      f: (A, B, C) => D): Option[D] =
        (oa, ob, oc) match
          case (Some(a), Some(b), Some(c)) => Some(f(a, b, c))
          case _ => None

  def map3ViaFor[A, B, C, D](oa: Option[A], ob: Option[B], oc: Option[C])(
      f: (A, B, C) => D): Option[D] =
        for {
          a <- oa
          b <- ob
          c <- oc
        } yield (f(a, b, c))

  def map3ViaFlatMap[A, B, C, D](oa: Option[A], ob: Option[B], oc: Option[C])(
      f: (A, B, C) => D): Option[D] =
        oa.flatMap( a => 
          ob.flatMap( b => 
            oc.map( c => 
              f(a, b, c)
            )
          )
        )

  // - Implement the traverse method.
  // - One interpretation of traverse is that is the same kind on method such as map but the function
  //   which transforms each element can fail. If any of the transformations fails, the transformation
  //   of the whole tree fails as well.
  // - Hint: read the solution of the midterm for inspiration
  def traverseOption[A, B](bt: BinaryTree[A])(f: A => Option[B]): Option[BinaryTree[B]] =
    bt.fold(Some(Empty))((l: Option[BinaryTree[B]], v: A, r: Option[BinaryTree[B]]) => {
      map3ViaMatch(l, f(v), r)(Branch(_, _, _))
    })

  // List

  // Define map2 for lists (the Scala ones)
  // One interpretation for List is that each represents a "single" value of a "non-deterministic" process.
  // For instance we can see tha list List(1, 2, 3, 4, 5, 6) as the "single" value representing the outcome
  // of the non-deterministic process of throwing a dice.
  // So
  //    val dice = List(1, 2, 3, 4, 5, 6)
  //    val sums = map2(dice, dice)(_ + _)
  // represents the "single" output of the non-deterministic process that throws three dices and sums them
  // Do it using high-level combinators such a map/flatMap or for-comprehensions

  def map2ViaMatch[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    def go[A, B](a: A, bs2: List[B])(f: (A, B) => C): List[C] = 
      bs2 match
        case b :: bbs => f(a, b) :: go(a, bbs)(f)
        case Nil => Nil

    (as, bs) match
      case (a::aas, b::bbs) => go(a, bs)(f) ++ map2ViaMatch(aas, bs)(f)
      case _  => List[C]()

  def map2ViaFor[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = 
    for {
      a <- as
      b <- bs
    } yield (f(a, b))

  def map2ViaFlatMap[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = 
    as.flatMap { a =>
      bs.map {
        b => f(a, b)
      }
    }
    
  // Apply the same idea to a map3
  def map3ViaMatch2[A, B, C, D](as: List[A], bs: List[B], cs: List[C])(f: (A, B, C) => D): List[D] = ???
  /*
    (as, bs, cs) match
      case (a::aas, b::bbs, c::ccs) => f(a, b, c) :: map3ViaMatch2(aas, bbs, ccs)(f)
      case _ => List[D]()
    */

  def map3ViaFor2[A, B, C, D](as: List[A], bs: List[B], cs: List[C])(f: (A, B, C) => D): List[D] = 
    for {
      a <- as
      b <- bs
      c <- cs
    } yield (f(a, b, c))

  def map3ViaFlatMap2[A, B, C, D](as: List[A], bs: List[B], cs: List[C])(f: (A, B, C) => D): List[D] = 
    as.flatMap{ a => 
      bs.flatMap{ b =>
        cs.map{ c => 
          f(a, b, c)
        }
      }
    }

  // Implement the traverse method but now for functions which return a non-deterministic value (a List[B])
  // In this interpretation the idea is that when we apply f to an A we get a non-deterministic value so the
  // result is a non-deterministic value of a tree, that is, all the possible trees I can create combining
  // the values that I get by the function at each position
  // Seems complicated? Yes, it does.
  // But the implementation is as simple as before.
  def traverse[A, B](bt: BinaryTree[A])(f: A => List[B]): List[BinaryTree[B]] =
    bt.fold(List(Empty)){
      (l: List[BinaryTree[B]], v: A, r: List[BinaryTree[B]]) => map3ViaFor2(l, f(v), r)(Branch(_, _, _))
    }

  // State
  import state.State
  import State.*

  // Let's do it one more time !!!
  // Define map3 but now for state actions

  //State[S, +A] = S => (A, S)

  //opaque type State[S, +A] = S => (A, S)

  def map3[S, A, B, C, D](sa: State[S, A], sb: State[S, B], sc: State[S, C])(
      f: (A, B, C) => D
  ): State[S, D] = 
    State {(s: S) => 
      val (a: A, s1: S) = sa.run(s)
      val (b: B, s2: S) = sb.run(s1)
      val (c: C, s3: S) = sc.run(s2)
      (f(a, b, c), s3)
    }

  def map3ViaApply[S, A, B, C, D](sa: State[S, A], sb: State[S, B], sc: State[S, C])(f: (A, B, C) => D): State[S, D] =
    State.apply {
      (s: S) => 
      val (a: A, s1: S) = sa.run(s)
      val (b: B, s2: S) = sb.run(s1)
      val (c: C, s3: S) = sc.run(s2)
      (f(a, b, c), s3)
    }
    

  // And now define traverse for functions which, at each element of the tree create an action that depends on it.
  // The resulting actions will construct a tree by traversing the initial tree of actions and using it to get
  // the element. The traversal threads the S along the tree.
  // Seems complicated? Yes, it does.
  // But the implementation is as simple as before.
  // Hint: Follow the types !!!!

  def traverseState[S, A, B](bt: BinaryTree[A])(f: A => State[S, B]): State[S, BinaryTree[B]] =
      bt.fold(State.apply((s: S) => (Empty: BinaryTree[B], s: S)))
              ((l: State[S, BinaryTree[B]], v: A, r: State[S, BinaryTree[B]]) => 
                map3(l, f(v), r)(Branch(_, _, _))) 
