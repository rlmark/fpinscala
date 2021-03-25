package fpinscala.state

case class Rand[A](stateTransition: RNG => (A, RNG))

object Rand {

  val int: Rand[Int] = Rand(rng => rng.nextInt)

  // Let's it obvious that this is a combinator (that is, a function that operates on higher order functions)
  val intEvenMoreObvious: Rand[Int] = {
    def getRandInt(rng: RNG): (Int, RNG) = {
      rng.nextInt
    }
    Rand(getRandInt)
  }

  def unit[A](a: A): Rand[A] = Rand(rng => (a, rng))

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {

    def stateTransition(rng: RNG): (B, RNG) = {
      val (nextA, nextRNG) = s.stateTransition(rng)
      (f(nextA), nextRNG)
    }

    Rand(stateTransition)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())){
      (currentRandAElement: Rand[A], accumulatedRandList: Rand[List[A]]) =>
        map2(currentRandAElement, accumulatedRandList){(a, listA) =>
          a :: listA
        }
    }
  }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      def stateTransition(rng: RNG): (C, RNG) = {
      // call the state transition function of the first Rand instance with our rng
      val (a, nextRNG1) = ra.stateTransition(rng)
      // call the next state
      val (b, nextRNG2) = rb.stateTransition(nextRNG1)
      (f(a,b), nextRNG2)
    }

    Rand(stateTransition)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    def stateTransition(rng: RNG): (B, RNG) = {
      val (a,nextRNG) = f.stateTransition(rng)
      val randB = g(a)
      randB.stateTransition(nextRNG)
    }
    Rand(stateTransition)
  }
}
