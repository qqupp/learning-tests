package learning.minijson

import learning.minijson.MiniJson.Json

trait Decoder[T] {
  def decode[T](j: Json): Either[String, T]
}

object Decoder {
  def apply[T](implicit D: Decoder[T]): Decoder[T] = D
}
