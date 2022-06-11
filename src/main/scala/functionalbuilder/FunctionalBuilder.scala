package functionalbuilder

import cats.{Applicative, Functor, Monad}
import cats._
import cats.data._
import cats.implicits._
import functionalbuilder.Transformable.Transformable

object Transformable extends LowerPriority {

  /*
     Typeclass that represents the ability for an entity A to be transformed into  C given you provide a component B
   */
  trait Transformable[A, B, C] {
    def transform(component: B, origin: A): C
  }

  implicit def baseCase[A, B, C]: Transformable[A => B, A, B] =
    new Transformable[A => B, A, B] {
      def transform(component: A, origin: A => B): B = origin(component)
    }

  implicit def baseCaseF[F[_]: Functor, A, B, C]: Transformable[F[A => B], A, F[B]] =
    new Transformable[F[A => B], A, F[B]] {
      override def transform(component: A, origin: F[A => B]): F[B] = origin.map(_(component))
    }

  implicit def baseCaseMAp[F[_]: Applicative, A, B, C]: Transformable[F[A => B], F[A], F[B]] =
    new Transformable[F[A => B], F[A], F[B]] {
      override def transform(component: F[A], origin: F[A => B]): F[B] = origin.ap(component)
    }

  /*
    Functional Builder
    creates an entity in steps

    Add the ability to provide arguments non in sequential order to higher order functions

    val f: Int => Boolean => Double => String =
      i => b => d => s"$i $b $d"

    val result: String =
      f
       .provide(4.0)
       .provide(false)
       .provide(10)

    val partialApplication: Int => Double => String =
      f.provide(false)
   */
  implicit class TransformableOps[A, B](f: A => B) {
    def provide[Z, C](component: Z)(implicit ev: Transformable[A => B, Z, C]): C = ev.transform(component, f)
    def provideM[F[_]: Applicative, Z, C](component: F[Z])(implicit ev: Transformable[A => B, Z, C]): F[C] =
      component.map(z => ev.transform(z, f))
  }

  implicit class TransformableOpsM[F[_]: Applicative, A, B](f: F[A => B]) {
    def provide[Z, C](component: Z)(implicit ev: Transformable[A => B, Z, C]): F[C] =
      f.map(ff => ev.transform(component, ff))

    def provideM[ZA, C](component: F[ZA])(implicit ev: Transformable[A => B, ZA, C]): F[C] =
      (f, component).mapN{  case (ff, z) => ev.transform(z, ff) }
  }

}

trait LowerPriority {

  implicit def inductiveCase[A, B, C, D](implicit
      tr: Transformable[B, C, D]
  ): Transformable[A => B, C, A => D] =
    new Transformable[A => B, C, A => D] {
      def transform(component: C, origin: A => B): A => D =
        a => tr.transform(component, origin(a))
    }

}
