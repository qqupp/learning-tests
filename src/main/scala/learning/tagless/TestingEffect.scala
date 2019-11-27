package learning.tagless

import cats._
import cats.data._
import cats.effect.{Bracket, ExitCase, Sync}
import cats.implicits._

class TestingEffect[TestEnv] {

  //type TestEffect[A] = State[TestEnv, Either[Throwable, A]]
  type StateTest[A] = State[TestEnv, A]
  type TestEffect[A] = EitherT[StateTest, Throwable, A]

  object MonadError {
    import cats.data.EitherT._
    val instance: MonadError[TestEffect, Throwable] = implicitly[MonadError[TestEffect, Throwable]]
  }

  object ApplicativeError {
    val instance: ApplicativeError[TestEffect, Throwable] = MonadError.instance
  }

  object Monad {
    val instance: Monad[TestEffect] = MonadError.instance
  }

  object Applicative {
    val instance: Applicative[TestEffect] = MonadError.instance
  }

  object Functor {
    val instance: Functor[TestEffect] = MonadError.instance
  }


  object Sync {
    val instance: Sync[TestEffect] = new Sync[TestEffect] {
      def suspend[A](thunk: => TestEffect[A]): TestEffect[A] =
        MonadError.instance.flatMap(MonadError.instance.unit)(_ => thunk)

      def bracketCase[A, B](acquire: TestEffect[A])(use: A => TestEffect[B])(release: (A, ExitCase[Throwable]) => TestEffect[Unit]): TestEffect[B] =
        for {
          a <- acquire
          eitEB <- MonadError.instance.attempt(use(a))
          _ <- release(a, ExitCase.attempt(eitEB))
        } yield eitEB match {
          case Right(b) => b
          case Left(e) => throw e
        }

      def flatMap[A, B](fa: TestEffect[A])(f: A => TestEffect[B]): TestEffect[B] =
        MonadError.instance.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => TestEffect[Either[A, B]]): TestEffect[B] =
        MonadError.instance.tailRecM(a)(f)

      def raiseError[A](e: Throwable): TestEffect[A] =
        MonadError.instance.raiseError[A](e)

      def handleErrorWith[A](fa: TestEffect[A])(f: Throwable => TestEffect[A]): TestEffect[A] =
        MonadError.instance.recoverWith(fa)({ case t: Throwable => f(t) })

      def pure[A](x: A): TestEffect[A] =
        MonadError.instance.pure(x)
    }
  }

  object Bracket {
    val instance: Bracket[TestEffect, Throwable] = Sync.instance
  }

  object Defer {
    val instance: Defer[TestEffect] = Sync.instance
  }
}

