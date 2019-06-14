package learning.cats.effect

import org.scalatest.{FlatSpec, Matchers}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.functor._


class IOAppSpecs extends FlatSpec with Matchers {

  "IOApp " should "contain a pure app" in new IOAppTestContext {

    val testApp = new IOApp {
      override def run(args: List[String]): IO[ExitCode] =
        IO(sideEffect()).map(_ => ExitCode.Success)
    }

    val io1: IO[ExitCode] = testApp.run(List())

    GLOBAL_VAR shouldBe 0
    io1.unsafeRunSync()
    GLOBAL_VAR shouldBe 1

  }

  it should "return an exit code" in new IOAppTestContext {

    val testApp = new IOApp {
      override def run(args: List[String]): IO[ExitCode] =
        IO(1).as(ExitCode(12))
    }

    // how do I test this?
    testApp.main(Array())
  }

  it should "not catch any thrown exception" in new IOAppTestContext {

    val testApp = new IOApp {
      override def run(args: List[String]): IO[ExitCode] =
        IO(exception()).as(ExitCode.Success)
    }

    assertThrows[TestException] {
      testApp.run(List()).unsafeRunSync()
    }


  }


  class TestException(msg: String) extends Exception

  trait IOAppTestContext {
    var GLOBAL_VAR = 0

    def sideEffect(): Unit = {
      GLOBAL_VAR += 1
    }

    def exception(): Unit = throw new TestException("Boom!")
  }
}
