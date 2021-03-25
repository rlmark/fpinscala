package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f)// Option[Option[B]]
      .getOrElse(None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this
      .map(i => Some(i)) // Option[Option[A]]
      .getOrElse(ob) // Option[B]
  }

  def filter(f: A => Boolean): Option[A] = {
    this flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object runOption extends App {
  val testOption1 = Some(1)
  val testNone: Option[Int] = None

  assert(testOption1.map(_+1) == Some(2))
  assert(testNone.map(_+1) == None)

  assert(testOption1.getOrElse(2) == 1)
  assert(testNone.getOrElse(2) == 2)

  assert(testOption1.flatMap(i => Some(i + 1)) == Some(2))
  assert(testOption1.flatMap(_ => None) == None)
  assert(testNone.flatMap(_ => Some(1)) == None)

  assert(testOption1.orElse(Some(2)) == Some(1))
  assert(testNone.orElse(Some(2)) == Some(2))

  assert(testOption1.filter(_ % 2 == 0) == None)
  assert(testOption1.filter(_ == 1) == Some(1))
  assert(testNone.filter(_ == 1) == None)

  println(Option.sequence(List(Some(1), Some(2))))
  println(Option.sequence(List(Some(1), None)))
  println(Option.sequence(List(None)))
  println(Option.sequence(List())) // Made a decision here....

  println(Option.traverse(List(1,2,3,4))(i => Some(i * 2)))
  println(Option.traverse(List(1,2,3,4))(i => if (i == 1) Some(i) else None ))
  println(Option.traverse[Int, Int](List())(i => Some(i))) // decisions
}

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = mean(xs)
    val maybeSeq: Option[Seq[Double]] = m.map(mean => xs.map(x => math.pow(x - mean, 2)))
    maybeSeq.flatMap(seq => mean(seq))
  }

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aPrime <- a
      bPrime <- b
    } yield f(aPrime, bPrime)
  }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List()))((maybeA,acc) =>
      maybeA.flatMap ( a =>
         acc.map(l => a :: l) // Option[List[A]]
      )
    )
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List()))((a, optionalListB) =>
      for {
        bs <- optionalListB
        b <- f(a)
      } yield b :: bs
    )
  }

}
