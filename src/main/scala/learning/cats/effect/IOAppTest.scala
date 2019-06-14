package learning.cats.effect

import cats.effect.{ExitCode, IO, IOApp, SyncIO}
import cats.syntax.functor._

object IOAppTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO(2).as(ExitCode(42))
}





object A1 extends App {

  def a: IO[String] = IO { println("start a"); println(s"end a"); "a" }
  def b(a: String): IO[String] = IO { println(s"b + $a"); while(true){println("o")}; s"b + $a" }
  import cats.syntax.functor._

  val r: SyncIO[Unit] = a.runAsync(e => IO(println((e.right.get))))

  r.unsafeRunSync()

  IO.pure(println("asdfasdf"))
}