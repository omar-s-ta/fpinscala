package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.state.State
import fpinscala.answers.monoids.{Foldable, Monoid}
import Applicative.Const
import fpinscala.answers.state
import fpinscala.answers.state

trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  type Id[A] = A
  object Id:
    given idApplicative: Applicative[Id] with
      def unit[A](a: => A): Id[A] = a
      override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] =
        fab(fa)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      fa.traverse[Id, B](f)(using Id.idApplicative)

    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      fa.traverse[Const[B, _], Nothing](f)

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, b) => ((), f(b, a)))(1)

    override def toList: List[A] =
      fa.mapAccum(List.empty[A])((a, s) => ((), a :: s))(1).reverse

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a =>
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield b)
        .run(s)

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, sIndex) => ((a, sIndex), sIndex + 1))(0)

    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, list) => (list.head, list.tail))(0)

    def zip[B](fb: F[B]): F[(A, B)] =
      fa.mapAccum(fb.toList):
        case (a, Nil) => sys.error("not same shape")
        case (a, (b :: tl)) => ((a, b), tl)
      .apply(0)

    def fuse[M[_], N[_], B](f: A => M[B], g: A => N[B])(using m: Applicative[M], n: Applicative[N])
      : (M[F[B]], N[F[B]]) =
      fa.traverse[[x] =>> (M[x], N[x]), B](a => (f(a), g(a)))(using m.product(n))

  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
    extension [A](fga: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        self.traverse(fga)(ga => ga.traverse(f))

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(List.empty[B]))((a, acc) => f(a).map2(acc)(_ :: _))

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        oa match
          case Some(a) => f(a).map(Some(_))
          case None => summon[Applicative[G]].unit(None)

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        f(ta.head).map2(ta.tail.traverse(_.traverse(f)))((h, t) => Tree(h, t))

  given mapTraverse[K]: Traverse[Map[K, _]] with
    extension [A](m: Map[K, A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
        m.foldRight(summon[Applicative[G]].unit(Map.empty[K, B])):
          case ((k, a), acc) =>
            f(a).map2(acc)((b, m) => m + (k -> b))
