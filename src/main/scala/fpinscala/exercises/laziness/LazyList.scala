package fpinscala.exercises.laziness

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

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def append[A2 >: A](other: => LazyList[A2]): LazyList[A2] =
    foldRight(other)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: LazyList[B]): Boolean = ???

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

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
