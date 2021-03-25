package fpinscala.errorhandling


import java.util.Locale

import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter
// 4.6
sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(a) => Right(f(a))
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(a) => f(a)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(_) => b
     case Right(a) => Right(a)
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   for {
     a <- this
     rightB <- b
   } yield f(a,rightB)
 }
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight(Right(List()):Either[E,List[B]]) { (a, acc) =>
      val eitherB: Either[E, B] = f(a)
      acc.map2(eitherB)((listB, b) => b :: listB) // Hey we can use map2 to implement Traverse!
    }
  }

  def traverseWithForExpression[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight(Right(List()):Either[E,List[B]]) { (a, acc) =>
      for {
        b <- f(a) // this gives you an Either[E,B], so on the left side of the for expression we have a B
        newListOfB <- acc // the type of our accumulator is Either[E,List[B]], so we unwrap the Either and get a list of B
      } yield b :: newListOfB // we cons the head to the tail (again foldRight operates on the last element in the list first
      // the return type of this entire expression is an Either
    }
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(identity) // recall that identity is a function that simply returns the element that is given as a parameter
    // so in this case, the identity function is a function from Either[E,A] => Either[E,A]
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

// 4.8 Either for Validation

case class Person(name: Name, age: Age)
case class Name(value: String)
case class Age(value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.") else Right(Name(name))
  def mkAge(age: Int): Either[String, Age] = if (age < 0) Left("Age is out of range.") else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person(_, _))

  def mkNameValidated(name: String): Validated[String, Name] =
    if (name == "" || name == null) Invalid(List("Name is empty.")) else Valid(Name(name))

  def mkAgeValidated(age: Int): Validated[String, Age] = if (age < 0) Invalid(List("Age is out of range.")) else Valid(Age(age))

  def mkPersonValidated(name: String, age: Int): Validated[String, Person] = mkNameValidated(name).map2(mkAgeValidated(age))(Person(_, _))

}

sealed trait Validated[E,A] {
  def map2[B, C ](vb: Validated[E,B])(f: (A, B) => C): Validated[E, C] = {
    val va: Validated[E, A] = this
    (va, vb) match {
      case (Valid(a), Valid(b)) => Valid(f(a,b)) // hey both of these are valid, apply function F and wrap it back into a Valid
      case (Valid(_), Invalid(e)) => Invalid(e) // one of these failed, return the invalid values
      case (Invalid(e), Valid(_)) => Invalid(e) // one of these failed, return the invalid values
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2) // BOTH of these failed, let's collect both errors
    }
  }
}

case class Invalid[E,A](errors: List[E]) extends Validated[E, A]
case class Valid[E,A](success: A) extends Validated[E, A]


object runPerson extends App {

  // this will fail fast!
  // FlatMap has FAIL FAST SEMANTICS
//  println(Person.mkPerson(null, -1))
//  println(Person.mkPerson("Name", -1)) // only if we succeed with the first validation can we fail the second

  // What would you need to change in order to report both errors?
  // You could represent the Left side as a List of Errors

  // Would you change map2 or the signature of mkPerson? = I would change the datatype I'm using to model this problem.
  // We still will use mk2 but our mkPerson function will return a different sealed trait

  // ENTER VALIDATED
//
  //  println(Person.mkPersonValidated(null, -1)) // both fail validation
  //  println(Person.mkPersonValidated(null, 1)) // name fails validation age passes
  //  println(Person.mkPersonValidated("name", -1)) // name passes validation, age fails
  //  println(Person.mkPersonValidated("name", 1)) // both name and age succeed

}
