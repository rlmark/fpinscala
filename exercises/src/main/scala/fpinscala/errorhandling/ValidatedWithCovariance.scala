package fpinscala.errorhandling

sealed trait ValidatedC[+E,+A] {
  // for hand wavey reasons EE has to be a supertype of E "Covariant type E occurs in Contravariant position ValidatedC[E,B] of vb"
  def map2[B, C, EE >: E](vb: ValidatedC[EE,B])(f: (A, B) => C): ValidatedC[EE, C] = {
    val va: ValidatedC[E, A] = this
    (va, vb) match {
      case (ValidC(a), ValidC(b)) => ValidC(f(a,b)) // hey both of these are valid, apply function F and wrap it back into a Valid
      case (ValidC(_), InvalidC(e)) => InvalidC(e) // one of these failed, return the InvalidC values
      case (InvalidC(e), ValidC(_)) => InvalidC(e) // one of these failed, return the InvalidC values
      case (InvalidC(e1), InvalidC(e2)) => InvalidC(e1 ++ e2) // BOTH of these failed, let's collect both errors
    }
  }

  def orElse[EE >: E, B >: A](b: => ValidatedC[EE, B]): ValidatedC[EE, B] = {
    this match {
      case InvalidC(e1) => b match {
        case InvalidC(e2) => InvalidC(e1 ++ e2) // Collect the errors if the second Validated is invalid
        case ValidC(b) => ValidC(b)
      }
      case ValidC(a) => ValidC(a)
    }
  }
  
}

object Validated2 {
  // (es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  def traverse[B,A,E](list: List[A])(f: A => ValidatedC[E,B]): ValidatedC[E, List[B]] = {

    list.foldRight(ValidC(List()): ValidatedC[E,List[B]]){(a, validatedListB) => 
      val validatedB: ValidatedC[E, B] = f(a)
      validatedB.map2(validatedListB){(b1, b2) => b1:: b2} // Note we deferred the hard stuff of managing the errors to map2 
    }
  }
}

case class InvalidC[E](errors: List[E]) extends ValidatedC[E, Nothing]
case class ValidC[A](success: A) extends ValidatedC[Nothing, A]




case class Person2(name: Name2, age: Age2)
case class Name2(value: String)
case class Age2(value: Int)

object Person2 {
  def mkName2Validated(n: String): ValidatedC[String, Name2] =
    if (n == "" || n == null) InvalidC(List("Name2 is empty.")) else ValidC(Name2(n))

  def mkAge2Validated(a: Int): ValidatedC[String, Age2] = if (a < 0) InvalidC(List(s"$a Age is out of range.")) else ValidC(Age2(a))

  def mkPerson2Validated(n: String, a: Int): ValidatedC[String, Person2] = mkName2Validated(n).map2(mkAge2Validated(a))(Person2(_, _))

}

object runValidatedCTests extends App {
  import Person2._
  val allInvalid: List[Int] = (1 to 4).map(_ * -1).toList

  println(Validated2.traverse(allInvalid)(ageInt => mkAge2Validated(ageInt)))
}
