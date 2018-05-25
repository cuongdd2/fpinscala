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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, next) = rng.nextInt
    if (i < 0) (i >>> 1, next) else (i, next)
  }

  def double(rng: RNG): (Double, RNG) = map(nonNegativeInt)(-1.0 * _ / Int.MinValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = both(_.nextInt, double)(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = both(double, _.nextInt)(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a => rng =>
      val (b, s) = rb(rng)
      (f(a, b), s)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => rng => (Nil, rng)
    case r :: rs => map2(r, sequence(rs))(_ :: _)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r2) = f(rng)
    g(a)(r2)
  }

  def nonNegLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) rng => (mod, rng)
    else nonNegLessThan(n)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] = rs match {
    case Nil => State(s => (Nil, s))
    case x :: xs => x.map2(sequence(xs))(_ :: _)
  }
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
    val newM = inputs.foldLeft(m)((mm, i) => i match {
      case _ if mm.candies == 0 => mm
      case Coin => Machine(false, mm.candies, mm.coins + 1)
      case Turn => if (mm.locked) mm else Machine(true, mm.candies - 1, mm.coins)
    })
    ((newM.candies, newM.coins), newM)
  })
}
