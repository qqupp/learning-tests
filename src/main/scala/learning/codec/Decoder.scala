package learning.codec

import cats._
import cats.implicits._

/*
  Generalized decoder from A to B with Error E
 */
final case class Decoder[-A, +E, +B](decode: A => Either[E, B]) {
  def apply(a: A): Either[E, B] = decode(a)

  def andThen[E1 >: E, C](decoder: Decoder[B, E1, C]): Decoder[A, E1, C] =
    Decoder { a => decode(a).flatMap( decoder.decode ) }

  def ignoreError: Decoder[A, Nothing, Option[B]] =
    Decoder { a => decode(a) match {
      case Left(_) => Right(None)
      case Right(b) => Right(Some(b))
      }
    }


  def toTraversableList: Decoder[List[A], E, List[B]] = toTraversable[List, A, B]

  def toTraversable[F[_] : Traverse, AA <: A, BB >: B]: Decoder[F[AA], E, F[BB]] =
    Decoder( (traversable: F[AA]) => traversable.traverse(decode(_)))

  def map[C](f: B => C): Decoder[A, E, C] =
    Decoder { a => decode(a).map(f) }

  def mapError[E1](f: E => E1): Decoder[A, E1, B] =
    Decoder { a => decode(a).left.map(f) }

  def flatMap[AA <: A, E1 >: E, C](f: B => Decoder[AA, E1, C]): Decoder[AA, E1, C] =
    Decoder { a =>
      decode(a) match {
        case Left(e) => Left(e)
        case Right(b) => f(b).decode(a)
      }
    }
}


object Decoder {
  def success[T](value: T): Decoder[Any, Nothing, T] = Decoder(_ => Right(value))
  def fail[T](value: T): Decoder[Any, T, Nothing] = Decoder(_ => Left(value))

  import cats._
  import cats.implicits._
  def traversingDecoder[F[_] : Traverse, A, E, B](decoder: Decoder[A, E, B]): Decoder[F[A], E, F[B]] =
    Decoder(traversable => traversable.traverse(decoder(_)))
}
