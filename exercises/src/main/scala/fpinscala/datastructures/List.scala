package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def head[A](list: List[A]):A = list match {
    case Cons(head, tail) => head
    case Nil => throw new NoSuchElementException("Hey, that's an empty list")
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => l // made an assumption here
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Nil, _) => Nil
      case (ls, 0) => ls
      case (Cons(_, tail), acc) => drop(tail, acc - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil // could throw here
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def length[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(_, t) => 1 + length(t)
    }
  }

  // 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // 3.11
  // sum
  def sumFoldL(ls: List[Int]): Int = {
    foldLeft(ls, 0)((acc, a) => acc + a)
  }
  // product
  def productFoldL(ls: List[Int]): Int = {
    foldLeft(ls, 1)((acc,next) => next * acc)
  }
  // length
  def lengthFoldL[A](ls: List[A]): Int = {
    foldLeft(ls, 0)((acc, _) => acc + 1)
  }

  // 3.12
  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, Nil: List[A])((b,a) => Cons(a,b))
  }

  // 3.13
  def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)( (b,a) => f(a,b))
  }
  def foldRightFromFoldL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, identity[B]_)((accumulatedBFunction, a) => (nextB: B) => f(a, accumulatedBFunction(nextB)))(z)
  }

  // 3.14 append w/ fold
  def appendFold[A](la: List[A], lb: List[A]): List[A] = {
    foldRight(la, lb)((a,seed) => Cons(a,seed))
  }

  def appen2d[A](l: List[A], a: A): List[A] = {
    foldRight(l, List(a))(Cons(_,_))
  }
  // 3.18 (non tail rec)
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h,t) => Cons(f(h), map(t)(f))
    }
  }
  // map with foldR
  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a,acc) => Cons(f(a), acc))
  }

  // 3.19 filter
  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A]){
      case(a,acc) if p(a) => Cons(a,acc)
      case(_,acc) => acc
    }
  }

  def scanRight[A,B](l: List[A],z:B)(f: (A, B) => B): List[B] = {
    foldRight(l, (z, List(z)))((a, b) => {
      val (curr, state) = b
      val nextB = f(a, curr)
      println(s"the arguments to function f are a: $a and b: $curr")
      println(s"current state after applying f is: ${nextB}")
      (nextB, Cons(nextB, state))
    })._2 // we take the 2nd element of the tuple because it's the List[B], the first argument is the current element in the list
  }

  def filterWithVisibilty[A](l: List[A])(p: A => Boolean): List[A] = {
    val collectedResults = scanRight(l,Nil: List[A]){(a: A, acc: List[A]) =>
      if (p(a)) {
        Cons(a, acc)
      } else acc
    }
    collectedResults match {
      case Nil => Nil
      case Cons(h,t) => h // just the first one is the answer
    }
  }

  def flatten[A](listOfLists: List[List[A]]):List[A] = listOfLists match {
    case Nil => Nil
    case (Cons(miniList, tail)) => append(foldRight(miniList, Nil: List[A])((a,b)=>Cons(a,b)), flatten(tail))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val listOfLists = foldRight(as, Nil: List[List[B]])((a, outputList) => Cons(f(a), outputList))
    flatten(listOfLists)
  }


  // Exercise 3.23
  def zipWith[A](list1 : List[A], list2: List[A])(f: (A, A) => A): List[A] = {
    def builder[A] (l1: List[A], l2: List[A], returnList: List[A] )(f: (A, A) => A): List[A] = {
      if (l1 == Nil || l2 == Nil) returnList
      else builder (tail (l1), tail (l2), Cons (f(head(l1), head(l2)), returnList))(f)
    }
    val backwardsList = builder(list1, list2, Nil)(f)
    reverse(backwardsList)
  }

}

object RunList extends App {

  import List._

  assert(tail(List(1, 2, 3)) == List(2, 3))
  assert(tail(List(1, 2)) == List(2))
  assert(tail(List()) == List())

  assert(setHead(List(2, 2, 3, 4), 1) == List(1, 2, 3, 4))
  assert(setHead(List(), 1) == List())
  assert(setHead(Nil, 1) == List())
  assert(setHead(List(), 1) == Nil)

  assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))
  assert(drop(List(1, 2, 3, 4), 4) == List())
  assert(drop(List(1, 2, 3, 4), 5) == List())
  assert(drop(List(), 4) == List())

  assert(dropWhile(List(2, 2, 2), (i: Int) => i == 2) == List())
  assert(dropWhile(List(2, 2, 3), (i: Int) => i == 2) == List(3))
  assert(dropWhile(List(2, 2, 3, 4), (i: Int) => i == 2) == List(3, 4))
  assert(dropWhile(List(), (i: Int) => i == 2) == List())

  assert(init(List(1,2,3,4,5)) == List(1,2,3,4))
  assert(init(List(1,2)) == List(1))
  assert(init(List()) == List())

  assert(length(List(1,2,3,4)) == 4)
  assert(length(List()) == 0)

  assert(foldLeft(List(1,2,3,4),0)((acc, _) => acc + 1) == 4)
  assert(foldLeft(List(1,2,3,4),0)((acc, next) => acc + next) == 10)

  println(foldRight(List(1,2,3), Nil: List[Int])((a,acc) => Cons(a, acc) ))
  println(foldRightTailRec(List(1,2,3), Nil: List[Int])((a,acc) => Cons(a, acc)))
  println(foldRightFromFoldL(List(1,2,3), Nil: List[Int])((a,acc) => Cons(a, acc)))

  println(foldRight(List(1,2,3,4), 1)((a,acc) => a * acc))
  println(foldRightTailRec(List(1,2,3,4), 1)((a,acc) => a * acc))
  println(foldRightFromFoldL(List(1,2,3,4), 1)((a,acc) => a * acc))

  assert(sumFoldL(List(0,1,2,3)) == 6)
  assert(sumFoldL(List()) == 0)

  assert(productFoldL(List(1,2,3,4)) == 24)
  assert(productFoldL(List()) == 1)

  assert(lengthFoldL(List(1,2,3,4,5,6)) == 6 )
  assert(lengthFoldL(List()) == 0)

  println(reverse(List(1,2,3,4,5)))

  println(appendFold(List(1,2,3,4), List(5,6,7,8)))

  assert(map(List(1,2,3,4))(_ + 1) == List(2,3,4,5))
  assert(map2(List(1,2,3,4))(_ + 1) == List(2,3,4,5))

//  println(filter(List(1,2,3,4,5,6,7,8))(_ % 2 == 0))
//  println(filter(List(1,1,1,1))(_ % 2 == 0))

  println(scanRight(List(1,2,3,4), 0)((a,b) => a + b))
//  println("this shows foldR execution")
//  println(scanRight(List(1,2,3,4), Nil: List[Int])((a,acc) => Cons(a, acc)))

//  println(filterWithVisibilty(List(1,2,3,4,5,6))(_ % 2 == 0))

}
