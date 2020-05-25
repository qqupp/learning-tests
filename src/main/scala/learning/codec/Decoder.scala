package learning.codec

/*
  Generalized decoder from A to B with Error E
 */
final case class Decoder[-A, +E, +B](decode: A => Either[E, B]) {
  def apply(a: A): Either[E, B] = decode(a)

  def andThen[E1 >: E, C](decoder: Decoder[B, E1, C]): Decoder[A, E1, C] =
    Decoder { a => decode(a).flatMap( decoder.decode ) }

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
}
