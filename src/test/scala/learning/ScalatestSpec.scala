package learning

import org.scalatest._

import scala.util.Try

class ScalatestSpec extends FlatSpec with Matchers {

  "option" should "be defined" in {
    donDoThis {
      aOption.isDefined shouldBe true
      //false was not equal to true
    }

    aOption shouldBe defined
    //None was not defined
  }

  it should "hvae a value" in {
    donDoThis {
      aOption.get shouldBe 1
      // no such element exception
    }

    import OptionValues._
    aOption.value shouldBe 1
  }

  "either" should "have a left value" in {
    donDoThis {
      anEither.left.get shouldBe "B"
    }

    import EitherValues._
    anEither.left.value shouldBe "B"
  }

  it should "have a right value" in {

    import EitherValues._
    anEither.right.value shouldBe 10
  }

  "collections" should "check empty" in {
    Collections.empty should not be empty
  }

  it should "contain" in {
    Collections.numbers should contain(3)
  }

  it should "have size" in {
    Collections.one should have size 2
  }

  it should "check forAll (fail fast)" in {
    import OptionValues._
    import Inspectors._
    forAll(Collections.options)(_.value shouldBe 1)
  }

  it should "check forEvery" in {
    import OptionValues._
    import Inspectors._
    forEvery(Collections.options)(_.value shouldBe 1)
  }

  "Exception" should "assertThrow" in {
    assertThrows[MyException2]{
      throw new MyException1("Boom!")
    }
  }

  it should "a exeption " in {

    a[MyException2] shouldBe thrownBy{
      throw new MyException1("Boom!")
    }
  }

  it should "intercept" in {
    val exn = intercept[MyException1](throw new MyException1("Boom!"))
    exn.getMessage shouldBe "Boom!!"
  }

  private def donDoThis[A](a: => A) = ()

  private val aOption: Option[Int] = None

  private val anEither: Either[String, Int] = Left("A")

  private object Collections {
    val empty = List()
    val one = List(1)
    val numbers = List(2, 4, 5, 6)
    val options = List(Some(2), None, Some(1), Some(3))
  }

  private class MyException1(override val getMessage: String) extends Exception
  private class MyException2(override val getMessage: String) extends Exception

}
