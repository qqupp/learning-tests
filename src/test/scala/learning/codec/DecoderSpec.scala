package learning.codec

import cats.Id
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
    val fstDecoder: Decoder[String, Throwable, Int] = str2Int
    val sndDecoder: Decoder[Int, PosIntException, PosInt] = int2PosInt
    val strng2PosInt: Decoder[String, Throwable, PosInt] = fstDecoder andThen sndDecoder

    val successResult = strng2PosInt("2")
    successResult shouldBe Right(PosInt.unsafeFrom(2))

    val failFstConversion = strng2PosInt("asdf")
    failFstConversion.left.get shouldBe a[java.lang.NumberFormatException]

    val failSndConversion = strng2PosInt("-10")
    failSndConversion.left.get shouldBe a[PosIntException]
  }

  it should "derive a traversableDecoder" in {
    val ListStr2ListInt: Decoder[List[String], Throwable, List[Int]] = str2Int.toTraversableList

    val csvStr2ListInt: Decoder[String, Throwable, List[Int]] = str2Csv andThen ListStr2ListInt

    csvStr2ListInt("2,45,-13,35") shouldBe Right(List(2,45,-13,35))
    csvStr2ListInt("2,45,Boom!,35").left.get shouldBe a[java.lang.NumberFormatException]
  }

  it should "remove the errors lifting the result to an option" in {
    val noErrorDecoder: Decoder[String, Nothing, Option[Int]] = str2Int.ignoreError

    noErrorDecoder("1") shouldBe Right(Some(1))
    noErrorDecoder("asdf") shouldBe Right(None)
  }

  lazy val str2Csv: Decoder[String, Nothing, List[String]] =
    Decoder( s => Right(s.split(",").toList) )

  lazy val str2Int: Decoder[String, Throwable, Int] =
    Decoder((str: String) => Try(str.toInt).toEither )

  import cats._
  import cats.implicits._
  lazy val lxxx: Decoder[List[String], Throwable, List[Int]] = Decoder(l => l.traverse(str2Int(_)))

  class PosIntException(msg: String) extends Exception(msg)
  sealed abstract case class PosInt(x: Int)
  object PosInt {
    def from(i: Int): Either[PosIntException, PosInt] =
      Either.cond(i > 0, new PosInt(i) {}, new PosIntException(s"cant build a pos int for $i"))

    def unsafeFrom(i: Int): PosInt = from(i).right.get
  }
  lazy val int2PosInt: Decoder[Int, PosIntException, PosInt] = Decoder((i: Int) => PosInt.from(i) )
}
