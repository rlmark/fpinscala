package fpinscala.state

import fpinscala.state.RNG.Rand

import scala.collection.immutable


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  // val int's type fully written out is RNG => (Int, RNG) so it is a function

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
  // unit is a weird name for this, it should not be confused with Scala's Unit we also call this kind of function "point" or "pure"

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def incorrectNonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInteger, randomNumberGenerator) = rng.nextInt
    (scala.math.abs(randomInteger), randomNumberGenerator) // Note this is technically a bit off because of the minimum int value
    // Copied from the javadocs underlying abs val impl:
    // Note that if the argument is equal to the value of
    //     * {@link Integer#MIN_VALUE}, the most negative representable
    //     * {@code int} value, the result is that same value, which is
    //     * negative.
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInteger, randomNumberGenerator) = rng.nextInt
    if (randomInteger < 0) {
      val avoidMinIntProblem = randomInteger + 1
      (avoidMinIntProblem * -1, randomNumberGenerator)
    } else (randomInteger, randomNumberGenerator)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, nextRNG) = nonNegativeInt(rng)
    val double = randomInt + randomInt / Int.MaxValue
    (double, nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // we're going to assume count is non negative, we could throw here if given a non negative number, or return empty list
    def loop(currentCount: Int, accumulator: List[Int], currentRNG: RNG): (List[Int], RNG) = {
      if (currentCount <= 0) (accumulator, currentRNG) else {
        val (nextInt, nextRNG) = nextInt(currentRNG)
        loop(currentCount - 1, nextInt::accumulator, nextRNG)
      }
    }

    loop(count, List(), rng)
  }

  // 6.5 double using map
  def doubleWithMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng : RNG => {
      // call the state transition function of the first Rand instance with our rng
      val (a, nextRNG1) = ra(rng)
      // get the next state by calling the rb function with our random number generator
      val (b, nextRNG2) = rb(nextRNG1)
      // return a tuple of C and the latest random number generator state
      (f(a,b), nextRNG2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())){
      (currentRandAElement, accumulatedRandList) =>
        map2(currentRandAElement,accumulatedRandList)((a,list) => a::list)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG => {
      val (a,nextRNG) = f(rng)
      val randB = g(a)
      randB(nextRNG)
    }
  }
}

// 6.10
case class State[S,+A](run: S => (A, S)) {



  def map[B](f: A => B): State[S, B] = {

    def stateTransition(initialState: S): (S, B) = {
      val (nextA, nextState) = this.run(initialState)
      val b = f(nextA)
      (nextState, b)
    }

    State(stateTransition)
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    def stateTransition(initialState: S): (S, C) = {
      // call the run function of this State instance with an initial state
      val (a, nextState1) = this.run(initialState)
      // call the next state using the state we get from the first run function
      val (b, nextState2) = sb.run(nextState1)
      // return the final state with the value of applying f to a and b
      (nextState2,f(a,b))
    }

    State(stateTransition)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    def stateTransition(initialState: S): (B, S) = {
      val (a, nextState) = this.run(initialState)
      val newStatefulTransition: State[S, B] = f(a)
      newStatefulTransition.run(nextState)
    }

    State(stateTransition)
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def pure[S,A](a: A): State[S,A] = State(rng => (a, rng))


  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    // the type we want
    val seedvalue: State[S,List[A]] = State((state: S) => (List.empty[A], state))

    fs.foldRight(seedvalue){
      (currentStateAElement: State[S, A], accumulatedStateList: State[S, List[A]]) =>
        currentStateAElement.map2(accumulatedStateList) { case (stateInList: A, accumulatedStateList: List[A]) =>
          stateInList::accumulatedStateList
        }
    }
  }

  def sequence2[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    // the type we want
    val seedvalue: State[S,List[A]] = State((state: S) => (List.empty[A], state))

    fs.foldRight(seedvalue){
      (currentStateAElement: State[S, A], accumulatedStateList: State[S, List[A]]) =>
        currentStateAElement.flatMap( a =>
        accumulatedStateList.map( listA => a :: listA))
    }
  }

  // stack overflow :(
  def sequenceOverflow[S, A](xs: List[State[S, A]]): State[S, List[A]] = {
    if (xs.isEmpty) State(s => (Nil, s))
    else {
      val tailSeq: State[S, List[A]] = sequence(xs.tail)
      xs.head.map2(tailSeq) { (a, b) => a :: b }
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
