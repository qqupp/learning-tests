package learning.minijson

import learning.minijson.MiniJson.Json

object Syntax {

  implicit final class EncoderOps[T](val t: T) extends AnyVal {
    def asJson(implicit E: Encoder[T]): Json = E.encode(t)
  }

  implicit final class DecoderOps(val j: Json) extends AnyVal {
    def as[T](implicit D: Decoder[T]): Either[String, T] = D.decode(j)
  }

}