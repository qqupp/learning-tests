package mapper

import cats._
import cats.implicits._
import mapper.DeepMapper.Mapper

object DeepMapper extends App {

  trait Mapper[A, B, C, D] {
    def mapper(a: A, f: B => C): D
  }

  implicit def fromFunctor[F[_], A, B](implicit
      ev: Functor[F]
  ): Mapper[F[A], A, B, F[B]] =
    new Mapper[F[A], A, B, F[B]] {
      def mapper(fa: F[A], f: A => B): F[B] = ev.fmap(fa)(f)
    }

  implicit def inductiveFunctorCase[F[_], A, B, C, D](implicit
      baseMapper: Mapper[A, B, C, D],
      functorF: Functor[F]
  ): Mapper[F[A], B, C, F[D]] =
    new Mapper[F[A], B, C, F[D]] {
      def mapper(a: F[A], f: B => C): F[D] =
        functorF.fmap(a)(e => baseMapper.mapper(e, f))
    }

  implicit class DeepMapOps[F[_], A](a: F[A]) {
    def deepMap[B, C, D](f: B => C)(implicit m: Mapper[F[A], B, C, D]): D =
      m.mapper(a, f)
  }

  // examples
  def d[T](t: T): List[T] = List(t, t)

  val l: List[List[List[List[Int]]]] = d(d(d(d(1))))

  def f: Int => String = (i: Int) => s"test ${i * 2}"

  // deep mapping manually
  val r0: List[List[List[List[String]]]] = l.map(_.map(_.map(_.map(f))))
  // with deepMap
  val r1: List[List[List[List[String]]]] = l.deepMap(f)

  println(r0)
  println(r1)

  println(List(1, 2, 3).deepMap(f))

  println(List(Some(1), None, Some(2)).deepMap(f))

  println(List(Right(Some(10)), Left("no"), Right(None)).deepMap(f))

}
