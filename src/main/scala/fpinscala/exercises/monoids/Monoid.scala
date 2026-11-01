package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    val empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    val empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val empty: Boolean = true

  case class MaybeSorted(sorted: Boolean, min: Int, max: Int)

  lazy val isSorted: Monoid[Option[MaybeSorted]] = new:
    val empty: Option[MaybeSorted] = None
    def combine(a1: Option[MaybeSorted], a2: Option[MaybeSorted]): Option[MaybeSorted] =
      (a1, a2) match
        case (Some(a), Some(b)) =>
          Some(MaybeSorted(a.sorted && b.sorted && a.max <= b.min, a.min, b.max))
        case (a, None) => a
        case (None, b) => b
        case _ => None

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    val empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    def empty: A => A = identity

  import fpinscala.exercises.testing.{Gen, Prop}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val id = Prop.forAll(gen)(a => m.combine(a, m.empty) == a && m.combine(m.empty, a) == a).tag("identity")
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case ((a, b), c) =>
          m.combine(m.combine(a, b), c) == m.combine(a, m.combine(b, c))
      .tag("associativity")

    id && associativity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length <= 1 then as.headOption.map(f).getOrElse(m.empty)
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def empty: Par[A] = Par.unit(m.empty)
    def combine(a1: Par[A], a2: Par[A]): Par[A] =
      a1.map2(a2)(m.combine)

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(bs => foldMapV(bs, par(m))(Par.lazyUnit))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, isSorted)(i => Some(MaybeSorted(true, i, i))).map(_.sorted).getOrElse(true)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
