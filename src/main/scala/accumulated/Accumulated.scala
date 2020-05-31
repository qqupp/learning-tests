package accumulated

import cats.Semigroup
import cats._
import cats.data._
import cats.implicits._

import Accumulated._

sealed abstract class Accumulated[+E, +O, +A] extends Product with Serializable {

  final def ap[EE >: E, OO >: O, C](
      f: Accumulated[EE, OO, A => C]
  )(implicit
      SemigroupB: Semigroup[OO],
      SemigroupE: Semigroup[EE]
  ): Accumulated[EE, OO, C] =
    (this, f) match {
      case (AccValid(a, b1), AccValid(ff, b2))      => AccValid(ff(a), SemigroupB.combine(b2, b1))
      case (AccValid(_, b1), AccInvalid(e2, b2))    => AccInvalid(e2, SemigroupB.combine(b2, b1))
      case (AccInvalid(e1, b1), AccInvalid(e2, b2)) =>
        AccInvalid(SemigroupE.combine(e1, e2), SemigroupB.combine(b2, b1))
      case (AccInvalid(e1, b1), AccValid(_, b2))    => AccInvalid(e1, SemigroupB.combine(b2, b1))
    }
//
//  final def map[B](f: A => B): Accumulated[E, O, B] =
//    this match {
//      case AccValid(a, o)       => AccValid(f(a), o)
//      case i @ AccInvalid(e, o) => i
//    }

}

object Accumulated {

  def valid[E]                     = new AccumulatedPartiallyAppliedValidBuilder[E] {}
  def invalid[E, O, A](e: E, o: O) = AccInvalid(e, o)

  class AccumulatedPartiallyAppliedValidBuilder[E] {
    def apply[O, A](a: A, o: O): Accumulated[E, O, A] = AccValid(a, o)
  }

  final case class AccValid[A, O](value: A, acc: O)   extends Accumulated[Nothing, O, A]
  final case class AccInvalid[E, O](error: E, acc: O) extends Accumulated[E, O, Nothing]

//  implicit def functorInstance[E, O]: Functor[Accumulated[E, O, ?]] =
//    new Functor[Accumulated[E, O, ?]] {
//      override def map[A, B](fa: Accumulated[E, O, A])(f: A => B): Accumulated[E, O, B] =
//        fa.map(f)
//    }

  implicit def applicativeInstance[E: Semigroup, O: Monoid]: Applicative[Accumulated[E, O, ?]] =
    new Applicative[Accumulated[E, O, ?]] {
      override def pure[A](x: A): Accumulated[E, O, A] = AccValid(x, implicitly[Monoid[O]].empty)

      override def ap[A, B](ff: Accumulated[E, O, A => B])(fa: Accumulated[E, O, A]): Accumulated[E, O, B] =
        fa.ap(ff)
    }

}
