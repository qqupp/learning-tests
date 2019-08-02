package learning.cats.effect

import cats.effect.{IO, SyncIO}
import org.scalatest.{FlatSpec, Matchers}

class IOSpecs extends FlatSpec with Matchers {

  "IO" should "run referentially transparently" in {

    val iou: IO[Unit] = IO.apply(println("hello!"))

    val program = for {
      _ <- iou
      _ <- iou
      _ <- iou
    } yield ()

    program.unsafeRunSync()
  }

  it should "attempt operations instead of throwing" in {

    def exception: String = throw new Exception("Boom!")

    val programWithAttempt: IO[Either[Throwable, String]] =
      for {
        e1 <- IO(exception).attempt
      } yield e1

    val programNoAttempt: IO[String] =
      for {
        e1 <- IO(exception)
      } yield e1

    val resultWithAttempt = programWithAttempt.unsafeRunSync()
    resultWithAttempt shouldBe a[Left[_, _]]

    assertThrows[Exception] {
      val resultNoAttempt = programNoAttempt.unsafeRunSync()
    }

  }

  it should "attempt different execption" in {

    trait TestExn extends Exception { val s: String }
    case class Exn1(s: String) extends TestExn
    case class Exn2(s: String) extends TestExn

    def exn1: String = throw Exn1("first")
    def exn2: String = throw Exn2("second")

    val program: IO[Either[Throwable, String]] =
      for {
        r1 <- IO(exn1).attempt
        r2 <- IO(exn2).attempt
        s <- IO(for {
          s1 <- r1
          s2 <- r2
        } yield s1 + s2)
      } yield s

    program.unsafeRunSync() shouldBe Left(Exn1("first"))
  }


  it should "run Async" in {

    def a: IO[String] = IO { println("start a"); println(s"end a"); "a" }
    def b(a: String): IO[String] = IO { println(s"b + $a"); s"b + $a" }
    import cats.syntax.functor._

    val r: Unit = {
      a.runAsync(e => b(e.right.get).as(Unit))
    }.unsafeRunSync()

  }

}
