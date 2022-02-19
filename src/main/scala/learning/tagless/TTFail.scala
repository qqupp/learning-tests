package learning.tagless

import cats.effect.{ExitCode, Fiber, IO, IOApp}

object TTFail extends  IOApp {

  val print: IO[Unit] = IO(println("OK"))

  val failZero: IO[Int] = IO(1 / 0)

  def run(args: List[String]): IO[ExitCode] =
    for {
      x <- failZero.start
      _ <- print
    } yield ExitCode.Success
}
