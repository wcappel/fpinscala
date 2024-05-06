package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val generated = rng.nextInt
    if generated._1 < 0 then (-(generated._1 + 1), generated._2)
    else generated

  def double(rng: RNG): (Double, RNG) =
    val (i, s)= nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), s)

  val _double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, s1) = nonNegativeInt(rng)
    val (d, s2) = double(s1)
    ((i, d), s2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), s) = intDouble(rng)
    ((d, i), s)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count > 1 then
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    else
      (List[Int](), rng)

  // ? Function infers parameter type on s0 of RNG since Rand output is defined as RNG => (A, RNG) ?
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    s0 =>
      val (a, s1) = ra(s0)
      val (b, s2) = rb(s1)
      (f(a, b), s2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs match
      case Nil => RNG => (Nil, RNG)
      case rx :: rt => map2(rx, sequence(rt))((x, xs) => x :: xs)

  def intsViaSequence(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    s0 =>
      val (a, s1) = r(s0)
      f(a)(s1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    val g: A => Rand[B] = a => unit(f(a))
    flatMap(r)(g)

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    val g: (A, B) => Rand[C] = (a, b) => unit(f(a, b))
    flatMap(ra)(a => flatMap(rb)(b => g(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s0 =>
        val (a, s1) = run(s0)
        (f(a), s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s0 =>
        val (a, s1) = run(s0)
        val (b, s2) = sb(s1)
        (f(a, b), s2)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s0 =>
        val (a, s1) = run(s0)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls match
      case Nil => State => (Nil, State)
      case s :: t => s.map2(sequence(t))((x, xs) => x :: xs)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    def update(i: Input, s: Machine): Machine =
      (i, s) match
        case (Input.Coin, Machine(true, candies, coins)) if candies > 0 => Machine(false, candies, coins + 1)
        case (Input.Turn, Machine(false, candies, coins)) if candies > 0 => Machine(true, candies - 1, coins)
        case _ => s
    
    for
      x <- State.sequence(inputs.map(i => State.modify(update(i, _))))
      s <- State.get
    yield (s.coins, s.candies)
