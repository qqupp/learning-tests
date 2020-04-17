package testing

import cats.effect.IO
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class PureTestSuiteExample extends PureTestSuite with Matchers with ScalaCheckDrivenPropertyChecks {

  spec("test simple data"){
    for {
      a <- IO("5")
      b <- IO(a.toInt)
      r <- IO(b - 2)
    } yield
      r shouldBe 1
  }


  forAll {
    (
      integer1: Int,
      integer2: Int
    ) =>
      spec(s"test int property: $integer1 $integer2") {
        for {
          p1 <- processInt(integer1)
          p2 <- processInt(integer2)
          result <- processInt(p1 + p2)
        } yield {
          result should be < 7
        }
      }
  }


  private def processInt(i: Int): IO[Int] = IO.pure(i)
}
