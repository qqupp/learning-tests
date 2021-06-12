package learning.codec.kleisli

import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.util.Try
import cats._
import cats.data._
import cats.implicits._

class DecoderWithKleisliTest extends AsyncFlatSpec with Matchers {

  import DecoderWithKleisli._
  val intDecoder: Decoder[String, Throwable, Int] = Decoder((s: String) => Try(s.toInt).toEither)

  it should "work" in {
    intDecoder("asdf").isLeft shouldBe true
    intDecoder("10") shouldBe Right(10)
  }

  val charDigit: Decoder[Char, Throwable, String]           =
    Decoder(c => Either.cond(c.isDigit, s"$c", new Throwable("Not a digit")))
  val secondChar: Decoder[String, Throwable, Char]          = Decoder.fromImpure(s => s.apply(1))
  val secondCahrNumDecoder: Decoder[String, Throwable, Int] =
    secondChar andThen charDigit andThen intDecoder

  it should "chain" in {
    secondCahrNumDecoder("").isLeft shouldBe true
    secondCahrNumDecoder("abc").isLeft shouldBe true
    secondCahrNumDecoder("a9b") shouldBe Right(9)
  }

  val boolDecoder: Decoder[String, Throwable, Boolean] = Decoder.fromImpure(s => s.toBoolean)

  trait MyDomain
  case class B(b: Boolean) extends MyDomain
  case class I(i: Int)     extends MyDomain

  it should "decode coproduct" in {
    val bDecoder: Decoder[String, Throwable, MyDomain] = boolDecoder.map(B(_))
    val iDecoder: Decoder[String, Throwable, MyDomain] = intDecoder.map(I(_))

    val myDomainDecoder: Decoder[String, Throwable, MyDomain] = bDecoder orElse iDecoder

    myDomainDecoder("false") shouldBe Right(B(false))
    myDomainDecoder("100") shouldBe Right(I(100))
    myDomainDecoder("asdfasdf").isLeft shouldBe true
  }

  case class Prod(b: Boolean, i: Int)

  it should "decode producs" in {
    def splitter(n: Int): Decoder[String, Throwable, String] = Decoder.fromImpure((_: String).split(",")(n))

    def prodDecoder =
      for {
        b <- splitter(0) andThen boolDecoder
        i <- splitter(1) andThen intDecoder
      } yield Prod(b, i)

    prodDecoder("false,10") shouldBe Right(Prod(false, 10))
  }
}
