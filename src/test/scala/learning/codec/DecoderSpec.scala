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
    val noErrorDecoder: Decoder[String, Nothing, Option[Int]] = str2Int.errorToOption

    noErrorDecoder("1") shouldBe Right(Some(1))
    noErrorDecoder("asdf") shouldBe Right(None)
  }

  it should "be created from a simple function a => b" in {
    val f: Int => String = _.toString

    val int2string: Decoder[Int, Throwable, String] = Decoder.fromImpure(f)
  }

  it should "be composable flatMap" in {
    val fstCharDecoder: Decoder[String, Throwable, Char] = Decoder.fromImpure(_.charAt(0))
    val sndCharDecoder: Decoder[String, Throwable, Char] = Decoder.fromImpure(_.charAt(1))

    val pair: Decoder[String, Throwable, (Char, Char)] =
      for {
        fst <- fstCharDecoder
        snd <- sndCharDecoder
      } yield (fst, snd)

    pair("12") shouldBe Right('1', '2')
    pair("") shouldBe a[Left[_, _]]
  }

  it should "be mappable" in {
    val d1: Decoder[Any, Nothing, Int] = Decoder.succeedWith(10)

    val d2: Decoder[Any, Nothing, String] = d1.map(_.toString)

    d2(Unit) shouldBe Right("10")
  }

  it should "be contramappable" in {
    val d1: Decoder[Int, Nothing, Int] = Decoder( x => Right(x + 1) )

    val d2: Decoder[Boolean, Throwable, Int] = d1.contraMap( b => if (b) 10 else 20 )

    d2(false) shouldBe Right(21)
  }

  it should "be rfinable in the error type" in {
    val fstCharDecoder: Decoder[String, Throwable, Char] = Decoder.fromImpure(_.charAt(0))

    val stringError: Decoder[String, String, Char] = fstCharDecoder.mapError(_ => "error type changed")
  }




  lazy val str2Csv: Decoder[String, Nothing, List[String]] =
    Decoder( s => Right(s.split(",").toList) )

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
