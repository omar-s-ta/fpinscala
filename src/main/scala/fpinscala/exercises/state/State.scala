package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
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
    val (n, next) = rng.nextInt
    if n < 0 then (-n, next) else (n, next)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match
      case (i, rng2) => (i % 2 == 0, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, next) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1), next)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, next1) = rng.nextInt
    val (d, next2) = double(next1)
    ((n, d), next2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), next) = intDouble(rng)
    ((d, n), next)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def _ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldLeft((List.empty[Int], rng)): (acc, _) =>
      val (n, r) = acc(1).nextInt
      (n :: acc(0), r)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def _double: Rand[Double] =
    map(nonNegativeInt)(n => (n.toDouble / (Int.MaxValue.toDouble + 1)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r0 =>
      val (a, r1) = ra(r0)
      val (b, r2) = rb(r1)
      (f(a, b), r2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    traverse(rs)(identity)

  def traverse[A, B](as: List[A])(f: A => Rand[B]): Rand[List[B]] =
    as.foldRight(unit(List.empty[B])): (a, acc) =>
      map2(f(a), acc)(_ :: _)

  def _flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (rb, rng1) = map(r)(f)(rng)
      rb(rng1)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng1) = r(rng)
      f(a)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def _map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- run
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = run(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f
  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
    traverse(as)(identity)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit(List.empty[B])): (a, acc) =>
      f(a).map2(acc)(_ :: _)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(input => State.modify(next(input)))
      state <- State.get
    yield (state.coins, state.candies)

  def next(input: Input)(machine: Machine): Machine =
    (input, machine) match
      case (_, Machine(_, 0, _)) => machine
      case (Input.Coin, Machine(false, _, _)) => machine
      case (Input.Turn, Machine(true, _, _)) => machine
      case (Input.Coin, Machine(true, _, _)) =>
        Machine(false, machine.candies, machine.coins + 1)
      case (Input.Turn, Machine(false, _, _)) =>
        Machine(true, machine.candies - 1, machine.coins)
