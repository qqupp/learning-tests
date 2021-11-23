import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Promise}
import scala.util.control.TailCalls.{TailRec, _}
import scala.util.{Failure, Success, Try}

final class IO[A] private (
    private val run: TailRec[A]
) {
  def flatMap[B](k: A => IO[B]): IO[B] =
    new IO(run.flatMap(a => k(a).run))

  def map[B](f: A => B): IO[B] =
    flatMap(f.andThen(IO.pure))

  def unsafeRun(): A = run.result
}

object IO {
  def delay[A](thunk: => A): IO[A] = new IO(done(()).flatMap(_ => done(thunk)))
  def pure[A](value: A): IO[A]     = new IO(done(value))

  trait Fiber[A] {
    protected val promise: Promise[A]

    // beware this is a busy wait blocking not a semantic blocking
    final def join: IO[A] =
      promise.future.value match {
        case Some(Success(a))         => IO.pure(a)
        case Some(Failure(exception)) => throw exception
        case None                     => join
      }
  }

  def start[A](io: IO[A])(implicit ec: ExecutionContext): IO[Fiber[A]] =
    for {
      prom <- IO.delay(Promise[A]())
      _    <- IO.delay(ec.execute(() => prom.complete(Try(io.unsafeRun()))))
    } yield new Fiber[A] { val promise = prom }

}
//
//object ExampleIOMonadMinimal extends App {
//
//  def output(s: String): IO[Unit] = IO.delay(println(s))
//  def input: IO[String]           = IO.delay(scala.io.StdIn.readLine())
//
//  val askName =
//    for {
//      _    <- output("hello!")
//      _    <- output("enter your name")
//      name <- input
//    } yield name
//
//  def greet(n: String) =
//    output(s"Greetings $n")
//
//  val askAndGreet =
//    for {
//      n <- askName
//      _ <- greet(n)
//    } yield n
//
//  def continue[A](io: IO[A]): IO[List[A]] =
//    for {
//      result <- io
//      _      <- output("Continue? [Y]/ N")
//      ans    <- input
//      rest   <- if (ans.toUpperCase == "N") IO.delay(List()) else continue(io)
//    } yield result :: rest
//
//  implicit val ec = ExecutionContext.global
//
//  val prog =
//    for {
//      f1    <- IO.start(count("AA____", 10, ""))
//      f2    <- IO.start(count("__BB__", 1000, ""))
//      f3    <- IO.start(count("____CC", 1000, ""))
//      _     <- output("before join")
//      rF1   <- f1.join
//      _     <- output("************ after join")
//      // _     <- output(s"$rF1")
//      names <- continue(askAndGreet)
//      _     <- output(s"you have greet ${names.size} times")
//      _     <- output(names.toString())
//      rF2   <- f2.join
//      _     <- output(s"$rF2")
//    } yield ()
//
//  def count(proc: String, n: Int, acc: String): IO[String] =
//    if (n <= 0) IO.pure(acc) else output(s"$proc current $n").flatMap(_ => count(proc, n - 1, s"$n; $acc"))
//
//  prog.unsafeRun
//}
