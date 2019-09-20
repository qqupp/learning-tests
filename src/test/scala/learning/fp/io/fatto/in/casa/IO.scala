package learning.fp.io.fatto.in.casa

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

trait IO[+A]
final case class Pure[A](value: A) extends IO[A]
final case class FlatMap[B, +A](io: IO[B], k: B => IO[A]) extends IO[A]
final case class Delay[+A](thunk: () => A) extends IO[A]
final case class Async[+A](k: (A => Unit) => Unit) extends IO[A]

object IO {
  def apply[T](t: => T): IO[T] = FFI.delay(t)
}

trait Fiber[F[_], T] {
  def join: F[T]
  def cancel: F[Unit]
}

/*
  FFI:  wrap side eff into IO
 */
object FFI {

  def delay[T](thunk: => T): IO[T] = Delay(() => thunk)

  // takes something that is already async and wraps it in IO
  // the magic is all in k!
  def async[T](k: (T => Unit) => Unit): IO[T] = Async(k)
  def asyncF[T](k: (T => Unit) => IO[Unit]): IO[T] = ???


  // submit the continuation to a thread pool
  def shift(implicit executionContext: ExecutionContext) = async{ callBack =>

    val runnable = new Runnable {
      override def run(): Unit = callBack()
    }

    executionContext.execute(runnable)
  }

  import Syntax._
  import Runners._
  def spawn[A](io: IO[A])(implicit executionContext: ExecutionContext): IO[A] = async{ callBack =>
    unsafeRunAsync(shift >> io)(_ => ())
    callBack()
  }
}

object Combinators {

  def pure[T](t: T): IO[T] = Pure(t)
  def map[A, B](fa: IO[A])(f: A => B): IO[B] = FlatMap(fa, (a: A) => pure(f(a)))
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)

  def start[A](fa: IO[A]): IO[Fiber[IO, A]] = ???
  def race[A, B](fa: IO[A], fb: IO[B]): IO[Either[A, B]] = ???
  def sleep(duration: FiniteDuration): IO[Unit] = ???
  def timeout[A](fa: IO[A])(duration: FiniteDuration): IO[Either[Unit, A]] = ???

}

object Syntax {

  implicit class IOOps[A](val io: IO[A]) extends AnyVal {
    def map[B](f: A => B): IO[B] = Combinators.map(io)(f)
    def flatMap[B](f: A => IO[B]): IO[B] = Combinators.flatMap(io)(f)
    def >>[B](fb: IO[B]): IO[B] = Combinators.flatMap(io)(_ => fb)
  }

}

object Stack {
  type Stack[T] = List[T]
  implicit class StackOps[T](val s: Stack[T]) extends AnyVal {
    def push(t: T): Stack[T] = t :: s
    def pop: Option[(T, Stack[T])] = s match {
      case Nil     => None
      case x :: xs => Some((x, xs))
    }
  }
}

object Runners {

  import Stack._
  import Combinators._

  def unsafeRunAsync[A](fa: IO[A])(cb: A => Unit): Unit = {

    def loop(current: IO[Any],
             callStack: Stack[Any => IO[Any]],
             callBack: A => Unit): Unit = {
      current match {
        case Async(k) =>
          val restOfComputation: Any => Unit = { res: Any =>
            val nextIO = pure(res)
            loop(nextIO, callStack, callBack)
          }
          // send computation somewhere Else
          k(restOfComputation)

        case FlatMap(io, k) =>
          loop(io, callStack.push(k), callBack)
        case Delay(body) =>
          val res = body()
          loop(pure(res), callStack, callBack)
        case Pure(v) =>
          callStack.pop match {
            case None => callBack(v.asInstanceOf[A])
            case Some((f, restStack)) =>
              val res = f(v)
              loop(res, restStack, callBack)
          }
      }
    }

    loop(fa, Nil, cb)
  }

  def unsafeRunSync[A](fa: IO[A]): A = {

    def loop(current: IO[Any], callStack: Stack[Any => IO[Any]]): A =
      current match {
        case FlatMap(io, k) =>
          loop(io, callStack.push(k))
        case Delay(body) =>
          val res = body()
          loop(pure(res), callStack)
        case Pure(v) =>
          callStack.pop match {
            case None => v.asInstanceOf[A]
            case Some((f, restStack)) =>
              val res = f(v)
              loop(res, restStack)
          }
      }

    loop(fa, List())
  }

}

trait IOApp {
  def ec: ExecutionContext
  def ses: ScheduledExecutorService
}

object SimpleProgram extends App {

  import Syntax._
  import Combinators._

  val read: IO[String] = IO(scala.io.StdIn.readLine())
  def put[T](v: T): IO[Unit] = IO(println(v))
  val prompt: IO[String] = put("Enter your name") >> read

  val program: IO[String] = prompt.flatMap(name => {
    put(s"Hello $name") >> pure(name)
  })

  val result = Runners.unsafeRunAsync(program)(v => println(s"you've got $v"))

}
