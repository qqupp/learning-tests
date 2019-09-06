package learning

import org.scalatest._

import scala.util.Try

class ScalatestSpec extends FlatSpec with Matchers {


  val aOption: Option[Int] = None

  "option" should "be defined" in {

    donDoThis{
      aOption.isDefined shouldBe true
      //false was not equal to true
    }

    aOption shouldBe defined
    //None was not defined
  }

  it should "hvae a value" in  {

    donDoThis{
      aOption.get shouldBe 1
      // no such element exception
    }

    import OptionValues._
    aOption.value shouldBe 1

  }


  val aEither: Either[String, Int] = Left("A")

  "either" should "have a left or right value" in {

    donDoThis{
      aEither.left.get shouldBe  "A"
    }

    import EitherValues._
    aEither.left.value shouldBe "A"
    aEither.right.value shouldBe 10
  }

  object Collections {
    val empty = List()
    val one = List(1)
    val numbers = List(2, 4, 5, 6)
    val options = List(Some(2), None, Some(1), Some(3))
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
    assertThrows[NullPointerException]( throw  new Exception("Boom!"))
  }

  it should "a exeption " in {
    a [NullPointerException] shouldBe thrownBy( throw  new Exception("Boom!"))
  }

  it should "intercept" in {
    val exn = intercept[Exception]( throw new Exception("Boom!"))
    exn.getMessage shouldBe "Boom!"
  }



  def donDoThis[A](a: => A) = ()
}
