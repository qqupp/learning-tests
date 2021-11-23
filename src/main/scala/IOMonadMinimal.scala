import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls._

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
}

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
//  val prog =
//    for {
//      names <- continue(askAndGreet)
//      _     <- output(s"you have greet ${names.size} times")
//      _     <- output(names.toString())
//    } yield ()
//
//  prog.unsafeRun
//}
