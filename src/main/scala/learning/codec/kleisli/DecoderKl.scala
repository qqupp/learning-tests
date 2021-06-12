package learning.codec.kleisli

import cats._
import cats.data._
import cats.implicits._

import scala.util.Try

object DecoderWithKleisli {

  //Input => Either[Error, Output]

  type Decoder[-Input, Error, Output] = Kleisli[Either[Error, *], Input, Output]

  object Decoder {
    def apply[I, E, O](f: I => Either[E, O]): Decoder[I, E, O] =
      Kleisli(f)

    def fromPure[I, O](f: I => O): Decoder[I, Nothing, O] =
      Kleisli(i => Right(f(i)))

    def fromImpure[I, O](f: I => O): Decoder[I, Throwable, O] =
      Kleisli(i => Try(f(i)).toEither)

  }

}
