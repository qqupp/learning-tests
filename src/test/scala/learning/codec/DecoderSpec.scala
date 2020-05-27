package learning.codec

import java.io

import cats.Id
import cats.data.Kleisli
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

  it should "traverse dropping" in {
    val strCsv2ListInt: Decoder[String, Nothing, List[Int]] =
      str2Csv andThen str2Int.errorToOption.toTraversableList.map(_.collect{ case Some(x) => x })

    strCsv2ListInt("1,2,3,4") shouldBe Right(List(1,2,3,4))
    strCsv2ListInt("1,EEEEE,2,ASDF,3,4") shouldBe Right(List(1,2,3,4))
  }

  it should "use orElse and whiden the type accordingly" in {
    sealed trait CharAt
    case class C10th(c: Char) extends CharAt
    case class C2nd(c: Char) extends CharAt
    val tenthChar: Decoder[String, Throwable, C10th] = Decoder.fromImpure(s => C10th(s.charAt(9)))
    val secondChar: Decoder[String, Throwable, C2nd] = Decoder.fromImpure(s => C2nd(s.charAt(1)))

    val tenthorsecondCharDecoder: Decoder[String, Throwable, CharAt] = tenthChar orElse(secondChar)

    tenthorsecondCharDecoder("") shouldBe a[Left[_, _]]
    tenthorsecondCharDecoder("0X2") shouldBe Right(C2nd('X'))
    tenthorsecondCharDecoder("0X2345678") shouldBe Right(C2nd('X'))
    tenthorsecondCharDecoder("012345678Y") shouldBe Right(C10th('Y'))
    tenthorsecondCharDecoder("012345678Y45678") shouldBe Right(C10th('Y'))
  }


  it should "decode example" in {

    val input = "ObjD(1,2,3)|  ObjA |    ObjB (false , 20   ) | ObjC(   true,2)"

    trait MyDomain
    case object ObjA extends MyDomain
    case class ObjB(b: Boolean, i: Int) extends MyDomain
    case class ObjC(b: Boolean, i: Int) extends MyDomain
    case class ObjD(i1: Int, i2: Int, i3: Int) extends MyDomain

    trait MyErrors
    case class LowError(error: String) extends MyErrors
    case class HigError(error: String) extends MyErrors

    val strToInt: Decoder[String, Throwable, Int] =
      Decoder.fromImpure( (s: String) => s.toInt )

    val strToBoolean: Decoder[String, Throwable, Boolean] =
      Decoder.fromImpure {
        case "true" => true
        case "false" => false
      }

    val trim: Decoder[String, Nothing, String] =
      Decoder( s => Right(s.trim) )

    def split(separator: Char): Decoder[String, Nothing, List[String]] =
      Decoder(input => Right(input.split(separator).toList))

    def selectArgs: Decoder[String, Throwable, String] =
      Decoder.fromImpure { s =>
        val `fst(` = s.indexOf('(')
        val `last)` = s.lastIndexOf(')')
        s.substring(`fst(` + 1, `last)`)
      }

    val aDecoder: Decoder[String, HigError, ObjA.type] =
      Decoder( s => Either.cond( s.startsWith("ObjA"), ObjA, HigError(s"$s not match")) )

    val bFunDecoder: Decoder[String, HigError, Boolean => Int => ObjB] =
      Decoder( s => Either.cond( s.startsWith("ObjB"), (ObjB.apply _).curried , HigError(s"$s not match")) )

    val cFunDecoder: Decoder[String, HigError, Boolean => Int => ObjC] =
      Decoder( s => Either.cond( s.startsWith("ObjC"), (ObjC.apply _).curried , HigError(s"$s not match")) )

    val dFunDecoder: Decoder[String, HigError, Int => Int => Int => ObjD] =
      Decoder( s => Either.cond( s.startsWith("ObjD"), (ObjD.apply _).curried , HigError(s"$s not match")) )

    def argSelector[T](args: List[T], selector: Int): Decoder[Any, LowError, T] =
      Decoder.fromImpure((_: Any) => args(selector)).mapError(_ => LowError(s"Cant select $selector from $args"))

    def argN

    val dDecoder: Decoder[String, MyErrors, ObjD] =
      (for {
        fn <- dFunDecoder
        args <- (selectArgs andThen split(',') andThen ( trim andThen strToInt ).toTraversableList)
                  .mapError(t => HigError("cant build arguments"))
        arg1 <- argSelector(args, 0)
        arg2 <- argSelector(args, 1)
        arg3 <- argSelector(args, 2)
      } yield fn(arg1)(arg2)(arg3))

    val bcDecoder: Decoder[String, MyErrors, MyDomain] = {
      for {
        fn <- (bFunDecoder orElse cFunDecoder)
        args <- (selectArgs andThen split(',') andThen trim.toTraversableList)
                  .mapError(t => HigError("cant build arguments") )
        arg1 <- argSelector(args, 0) andThen strToBoolean.mapError(e => LowError(s"$e"))
        arg2 <- argSelector(args, 1) andThen strToInt.mapError(e => LowError(s"$e"))
      } yield fn(arg1)(arg2)
    }

    val domainDecoder: Decoder[String, MyErrors, MyDomain] = (aDecoder orElse bcDecoder orElse dDecoder)

    val strToDomainObjListDecoder: Decoder[String, MyErrors, List[MyDomain]] = {
      split('|') andThen trim.toTraversableList andThen domainDecoder.toTraversableList
    }

    strToDomainObjListDecoder(input) shouldBe 1
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
