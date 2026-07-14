package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{ExecutorService, Executors}
import fpinscala.exercises.testing.Prop.Result.Falsified
import annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true

  extension (self: Prop)
    def run(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Result.Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Result.Passed =>
          println(s"+ OK, passed $testCases tests.")

  extension (self: Prop)
    def check(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("and-left")(max, n, rng) match
          case Result.Passed => that.tag("and-right")(max, n, rng)
          case failed => failed

  extension (self: Prop)
    def ||(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("or-left")(max, n, rng) match
          case Falsified(_, _) => that.tag("or-right")(max, n, rng)
          case passed => passed

  extension (self: Prop)
    def tag(msg: String): Prop =
      (max, n, rng) =>
        self(max, n, rng) match
          case Falsified(failure, successes) => Falsified(FailedCase.fromString(s"$msg($failure)"), successes)
          case passed => passed

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      randomLazyList(g)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Result.Passed
              else Falsified(a.toString, i)
            catch
              case e: Exception =>
                Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Result.Passed)

  
  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

// S => (A, S)
// RNG => (A, RNG)
opaque type Gen[+A] = State[RNG, A]

object Gen:
  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG.boolean)

  def double: Gen[Double] =
    State(RNG.double)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def union[A](ga: Gen[A], gb: Gen[A]): Gen[A] =
    boolean.flatMap: a =>
      if (a) then ga else gb

  def weighted[A](ga: (Gen[A], Double), gb: (Gen[A], Double)): Gen[A] =
    val aBound = ga(1).abs / (ga(1).abs + gb(1).abs)
    double.flatMap: d =>
      if d < aBound then ga(0) else gb(0)

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def unsized: SGen[A] = _ => self

    def list: SGen[List[A]] = n => listOfN(n)

    def nonEmptyList: SGen[List[A]] = n => listOfN(n.max(1))

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(size: Int): Gen[A] = self(size)
    def map[B](f: A => B): SGen[B] = n => self(n).map(f)
    def flatMap[B](f: A => SGen[B]): SGen[B] = n => self(n).flatMap(f(_)(n))

@main def sortedProperty: Unit =
  val ints = Gen.choose(1, 10)
  val sortedProp = Prop.forAll(ints.nonEmptyList): lst =>
    val st = lst.sorted
    st.nonEmpty && st.zip(st.tail).forall((a, b) => a <= b)

  sortedProp.run()
