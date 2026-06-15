package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop:
  def &&(that: Prop): Prop

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

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

  extension [A](self: Gen[A]) def next(rng: RNG): (A, RNG) = self.run(rng)

  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

  extension [A](self: Gen[A])
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

// trait Gen[A]:
//   def map[B](f: A => B): Gen[B] = ???
//   def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
