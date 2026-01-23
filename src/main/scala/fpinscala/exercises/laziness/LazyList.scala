package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    this match
      case Empty => List.empty[A]
      case Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => LazyList.cons(h(), t().take(n - 1))
      case _ => LazyList.empty

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this

  def takeWhilePatterMatch(p: A => Boolean): LazyList[A] =
    this match
      case Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
      case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None)((a, _) => Some(a))

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()) -> Some(hb())), (ta() -> tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()) -> None), (ta() -> Empty))
      case (Empty, Cons(hb, tb)) => Some((None -> Some(hb())), (Empty -> tb()))
      case _ => None

  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    zipWith(that)(_ -> _)

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => f(a).append(b))

  def startsWithPatterMatching[B](that: LazyList[B]): Boolean =
    (this, that) match
      case (Cons(ha, ta), Cons(hb, tb)) => if ha() == hb() then ta().startsWith(tb()) else false
      case (Empty, Cons(_, _)) => false
      case _ => true

  def startsWith[B](that: LazyList[B]): Boolean =
    zipAll(that).takeWhile(_(1).isDefined).forAll(_ == _)

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case list @ Cons(_, _) => Some((list, list.t()))
      case Empty => None
    .append(LazyList(LazyList.empty))

  def hasSubsequence[A](list: LazyList[A]): Boolean =
    tails.exists(_.startsWith(list))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b) =>
      lazy val accB = b
      val rb = f(a, accB(0))
      (rb, LazyList.cons(rb, accB(1)))
    ._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = {
    lazy val singleton: LazyList[A] = LazyList.cons(a, singleton)
    singleton
  }

  def from(n: Int): LazyList[Int] =
    LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def f(a: Int, b: Int): LazyList[Int] =
      LazyList.cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => LazyList.cons(a, unfold(s)(f))
      case None => LazyList.empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)): (a, b) =>
      Some(a, (b, a + b))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(v => Some(v, v + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))
