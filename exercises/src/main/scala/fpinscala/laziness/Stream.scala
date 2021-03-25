package fpinscala.laziness

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  @tailrec
  final def foldLeft[B](z: => B)(f: (=> B, => A) => B): B = {
    this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.1
  def toList: List[A] = {
    this.foldRight(List.empty[A])((i, acc) => i :: acc)
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    @tailrec
    def loop(counter: Int, acc: Stream[A], original: Stream[A]): Stream[A] = {
      (counter, original) match {
        case (_, Empty) => acc
        case (0, _) => acc
        case (n, Cons(h, t)) => loop(n - 1, Cons(h, () => acc), t()) // Question to the group: what does calling t() here do?
      }
    }

    loop(n, Stream.empty[A], this)
  }

  def drop(n: Int): Stream[A] = {
    def loop(counter: Int, original: Stream[A]): Stream[A] = {
      (counter, original) match {
        case (_, Empty) => Empty
        case (0, _) => original
        case (n, Cons(_, t)) => loop(n - 1, t())
      }
    }

    loop(n, this)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(acc: Stream[A], original: Stream[A]): Stream[A] = {
      original match {
        case Empty => acc
        case Cons(h, _) if !p(h()) => acc
        case Cons(h, t) if p(h()) => loop(Cons(h, () => acc), t())
      }
    }

    loop(Stream.empty[A], this)
      .foldLeft(Stream.empty[A])((acc, i) => Cons(() => i, () => acc)) // Without the foldLeft it's backwards
  }

  def takenWhile2(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) => {
        lazy val head = h() // Only call it once, and memoize it
        if (p(head)) Cons(() => head, () => t().takenWhile2(p)) else t().takenWhile2(p)
      }
      case _ => Empty
    }
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((elem, bool) => p(elem) && bool)
  }

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Cons(() => f(a), () => b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)
  }

  def append[AA >: A](that: Stream[AA]): Stream[AA] = {
    foldRight(that)((a, acc) => Cons(() => a, () => acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, bs) => f(a).append(bs))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def zip[B](bs: Stream[B]): Stream[(A, B)] = {
    def loop(acc: Stream[(A, B)], original: Stream[A], bs: Stream[B]): Stream[(A, B)] = {
      (original, bs) match {
        case (Empty, _) => acc
        case (_, Empty) => acc
        case (Cons(a, tailA), Cons(b, tailB)) => loop(Cons(() => (a(), b()), () => acc), tailA(), tailB())
      }
    }

    loop(Stream.empty, this, bs)
  }

  def tail: Stream[A] = {
    this match {
      case Cons(_, tail) => tail()
      case Empty => Empty
    }
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        val newB: B = f(h())
        Some(newB, t())
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    Stream.unfold((this,n)){
      case (Cons(h,t), count) if count > 0 => Some((h(), (t(), count - 1)))
      case _ => None
    }

  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this){
      case Cons(h,t) =>
        lazy val head = h()
        if (p(head)) Some((head, t())) else None
      case Empty => None
    }
  }

    def zipWithUnfold[AA >: A](stream2: Stream[AA])(f: (AA, AA) => AA): Stream[AA] = {
      Stream.unfold((this, stream2)){
        case(Cons(head1, tail1), Cons(head2, tail2)) =>
          val newElement = f(head1(), head2())
//          println(s"${head1()}, ${head2()} new ${newElement}")
          Some(newElement, (tail1(), tail2()))
        case _ => None
      }
    }

  def zipAll[B](stream2: Stream[B]): Stream[(Option[A],Option[B])] = {
    Stream.unfold((this, stream2)){
      case (Empty, Empty) => None
      case (Cons(head1, tail1), Empty) =>
        val nextElementTuple: (Option[A], Option[B]) = (Some(head1()), None)
        val nextStateTuple: (Stream[A], Stream[B]) = (tail1(), Empty)
        Some((nextElementTuple, nextStateTuple))
      case (Empty, Cons(head2, tail2)) =>
        val nextElementTuple: (Option[A], Option[B]) = (None, Some(head2()))
        val nextStateTuple: (Stream[A], Stream[B]) = (Empty, tail2())
        Some(nextElementTuple, nextStateTuple)
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        val nextElementTuple: (Option[A], Option[B]) = (Some(head1()), Some(head2()))
        val nextStateTuple: (Stream[A], Stream[B]) = (tail1(), tail2())
        Some(nextElementTuple, nextStateTuple)
    }
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  // 5.8 (constant), 5.9 (from), 5.10 (fibs),  5.11 (unfold), and 5.13 (practice using unfold)

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1)) /// backwards. flatMap will stack overflow but operates in the right order
  }

  //  def fromNotBackwards(n: Int): Stream[Int] = {
  //    from(n).foldLeft(Stream[Int]()){case (b, i) => cons(i, b)}
  //  } this one never terminates

  lazy val fibs1: Stream[Int] = { // NOPE never terminates
    Stream(0, 1).zip(fibs1.tail).map { case (first, second) => first + second }
  }

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, loop(b, a + b))
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((element, nextS)) =>
        println(s"the element in the stream is $element")
        Stream.cons(element, unfold(nextS)(f))
      case None =>
        println(s"None returned applying function f to $z")
        Stream.empty
    }
  }

  def unfoldOnes: Stream[Int] = {
    unfold(1)(one => Some(one, 1))
  }

  def unfoldConst[A](a: A): Stream[A] = {
    unfold(a)(elem => Some(a, elem))
  }

  def unfoldFrom(n: Int): Stream[Int] = {
    unfold(n)(integer => Some(integer, integer + 1))
  }


}

object RunStream extends App {
  val testStream = Stream(1, 2, 3, 4, 5)

  assert(testStream.toList == List(1, 2, 3, 4, 5))
  assert(Stream.empty.toList == List())

  // println(testStream.foldLeft(Stream.empty[Int])((acc, i) => Cons(() => i, () => acc)).toList)

  assert(testStream.take(6).toList == List(5, 4, 3, 2, 1))
  assert(testStream.take(5).toList == List(5, 4, 3, 2, 1))
  assert(testStream.take(4).toList == List(4, 3, 2, 1))
  assert(testStream.take(0).toList == List())

  assert(testStream.drop(6).toList == List())
  assert(testStream.drop(5).toList == List())
  assert(testStream.drop(3).toList == List(4, 5))
  assert(testStream.drop(0).toList == List(1, 2, 3, 4, 5))

  assert(testStream.takeWhile(_ <= 3).toList == List(1, 2, 3))
  assert(testStream.takenWhile2(_ <= 3).toList == List(1, 2, 3))


  assert(testStream.forAll(_ <= 10) == true)
  assert(testStream.forAll(_ <= 3) == false)

  assert(testStream.map(_ + 1).toList == List(2, 3, 4, 5, 6))

  assert(testStream.filter(_ % 2 == 0).toList == List(2, 4))

  assert(testStream.flatMap(a => Stream(a)).toList == List(1, 2, 3, 4, 5))

  assert(Stream.constant(4).take(3).toList == List(4, 4, 4))

  //  println(Stream.from(4).take(4).toList) // hmm
  //  println(Stream.fibs.take(6).toList)

  //  println(Stream.unfold(0)(int => Option(int, int + 1)).take(6).toList)
  //  println(Stream.unfold(0)(int => if (int < 10) Some(int, int + 1) else None).take(20).toList)
  //
  //  println(Stream.unfoldOnes.take(4).toList)
  //  println(Stream.unfoldConst("a").take(4).toList)
  //  println(Stream.unfoldFrom(1).take(4).toList)

  //  println(testStream.mapUnfold(int => int.toString).toList)
  //  println(testStream.takeUnfold(3).toList)
  //  println(testStream.takeWhileUnfold( _ < 3).toList)

  println(testStream.zipWithUnfold(testStream.takeUnfold(3))((longerStream, shorter) => longerStream * shorter).toList)
  println(testStream.zipAll(testStream.takeUnfold(3)).toList)
}
