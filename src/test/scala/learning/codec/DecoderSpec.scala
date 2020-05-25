package learning.codec

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class DecoderSpec extends FlatSpec with Matchers {

  "A decoder" should "decode" in {
    val failedResult = str2Int("abc")
    failedResult shouldBe a[Left[_, _]]

    val successResult = str2Int("2")
    successResult shouldBe a[Right[_, _]]
  }

  lazy val str2Int: Decoder[String, Throwable, Int] =
    Decoder((str: String) => Try(str.toInt).toEither )
}
