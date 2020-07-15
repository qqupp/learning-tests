package learning.minijson

import learning.minijson.MiniJson.Json

trait Encoder[T] {
  def encode[T](t: T): Json
}

object Encoder {
  def apply[T](implicit E: Encoder[T]): Encoder[T] = E
}
