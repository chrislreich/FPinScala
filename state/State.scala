package fpinscala.state


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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt


    if (i == Int.MinValue) (Int.MaxValue, rng2)
    else if (i < 0)  (i * (-1), rng2)
    else (i, rng2)


  }


  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)

    if (i1 == Int.MaxValue) ((i1 - 1)/(Int.MaxValue.toDouble), rng2)
    else (i1/Int.MaxValue.toDouble, rng2)
  }


  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)

    ((i1,d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (i1, rng3) = rng2.nextInt

    ((d1, i1), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1,d2,d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List(), rng)
    else {
      val (i, rng2) = rng.nextInt
      val (seq, rng3) = ints(count - 1)(rng2)
      (i :: seq, rng3)
    }
  }

  // 6.5
  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(x => x / Int.MaxValue.toDouble)
  }

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      {
        val (a1, rng2) = ra(rng)
        val (a2, rng3) = rb(rng2)

        (f(a1, a2), rng3)
      }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => fs match {
    case x :: xs => {
      val (a1, rng2) = x(rng)
      val (a2, rng3) = sequence(xs)(rng2)
      (a1 :: a2, rng3)
    }
    case _ => (List(), rng)
  }

  def intsViaSequence(n : Int) : Rand[List[Int]] = {
    sequence(List.fill(n)(int))
  }


  def sequenceViaFoldRight[A](fs : List[Rand[A]]) : Rand[List[A]] = {
      fs.foldRight(unit(List[A]())) ((a, b) => map2(a,b)(_ :: _))
  }


  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a1, rng2) = f(rng)
      g(a1)(rng2)
    }
  }


  def nonNegativeLessThanViaFlatMap(n : Int) : Rand[Int] = {
    flatMap(nonNegativeInt)(
      x => {
        val mod = x % n
        if (x + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThanViaFlatMap(n)
    })
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) (x => rng1 => (f(x), rng1))
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
  }

}
case class State[S,+A](run: S => (A, S)) {




  def map[B](f: A => B): State[S, B] =  {
    State(x => {
      val (a1, s) = this.run(x)
      (f(a1), s)
  })

  }



  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(x => {
      val (a1, s1) = this.run(x)
      val (a2, s2) = sb.run(s1)
      (f(a1, a2), s2)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(x => {
      val (a1, s) = this.run(x)
      f(a1).run(s)
    })
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[A, S](a: A) : State[S, A] = {
    State(x => (a, x))
  }

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[List[A], S](List[A]()))((a,b) => a.map2(b)(_ :: _))
  }


  type Rand[A] = State[RNG, A]

  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(
    machine => (inputs, machine) match {
      case (_, Machine(lock, 0, coins)) => ((coins, 0), machine)
      case ((x :: xs), Machine(true, candy, coins)) => x match {
        case Coin => simulateMachine(xs).run(Machine(false, candy, coins + 1))
        case Turn => simulateMachine(xs).run(machine)
      }
      case ((x :: xs), Machine(false, candy, coins)) => x match {
        case Coin => simulateMachine(xs).run(machine)
        case Turn => simulateMachine(xs).run(Machine(true, candy - 1, coins))
      }
      case (_, Machine(lock, candy, coins)) => ((coins, candy), machine)
    }
  )
}
