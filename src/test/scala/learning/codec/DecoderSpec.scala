package learning.codec

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class DecoderSpec extends FlatSpec with Matchers {

  "A decoder" should "decode" in {
    val failedResult = str2Int("abc")
    failedResult shouldBe a[Left[_, _]]

    val successResult = str2Int("2")
    successResult shouldBe Right(2)
  }

  it should "compose and shortcut errors" in {
    val strng2PosInt: Decoder[String, Throwable, PosInt] = str2Int andThen int2PosInt

    val successResult = strng2PosInt("2")
    successResult shouldBe Right(PosInt.unsafeFrom(2))

    val failFstConversion = strng2PosInt("asdf")
    failFstConversion.left.get shouldBe a[java.lang.NumberFormatException]

    val failSndConversion = strng2PosInt("-10")
    failSndConversion.left.get shouldBe a[PosIntException]
  }

  lazy val str2Int: Decoder[String, Throwable, Int] =
    Decoder((str: String) => Try(str.toInt).toEither )


  class PosIntException(msg: String) extends Exception(msg)
  sealed abstract case class PosInt(x: Int)
  object PosInt {
    def from(i: Int): Either[PosIntException, PosInt] =
      Either.cond(i > 0, new PosInt(i) {}, new PosIntException(s"cant build a pos int for $i"))

    def unsafeFrom(i: Int): PosInt = from(i).right.get
  }
  lazy val int2PosInt: Decoder[Int, PosIntException, PosInt] = Decoder((i: Int) => PosInt.from(i) )
}
