package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{dual, endoMonoid}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldMap(f.curried)(using endoMonoid[B])(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldMap(a => b => f(b, a))(using dual(endoMonoid[B]))(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldLeft(mb.empty)((b, a) => mb.combine(b, f(a)))

    def combineAll(using ma: Monoid[A]): A =
      as.foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      as.foldRight(List.empty[A])(_ :: _)

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Branch, Leaf}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case Leaf(value) => f(value, acc)
        case Branch(left, right) => left.foldRight(right.foldRight(acc)(f))(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case Leaf(value) => f(acc, value)
        case Branch(left, right) => right.foldLeft(left.foldLeft(acc)(f))(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Leaf(value) => f(value)
        case Branch(left, right) => mb.combine(left.foldMap(f), right.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case Some(value) => f(value, acc)
        case None => acc

      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case Some(value) => f(acc, value)
        case None => acc

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Some(value) => f(value)
        case None => mb.empty
