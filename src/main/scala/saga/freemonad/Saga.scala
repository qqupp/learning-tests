package saga.freemonad

import cats.data.StateT
import cats.free.Free
import cats.{Monad, effect, ~>}
import cats._
import cats.data._
import cats.implicits._

import scala.util.Random

/*
 An implementation of the saga pattern using a free monad
 */
final class SagaBuilder[F[_]: Monad, E] { self =>

  case class Saga[A](localTransact: F[Either[E, A]], localCompensate: E => F[E])

  type SagaMonad[A] = Free[Saga, A]

  def saga[A](localTransact: F[Either[E, A]], localCompensate: E => F[E]): SagaMonad[A] =
    Free.liftF(Saga(localTransact, localCompensate))

  def rollback(localCompensate: F[E]): SagaMonad[Unit] =
    Free.liftF(Saga[Unit](localCompensate.map(Either.left(_)), e => Monad[F].pure(e)))

  def transact[A](saga: SagaMonad[A]): F[Either[E, A]] = {
    val transacted = saga.foldMap(compiler).value
    transacted.runA(Coordinator())
  }

  /*
   * Private Stuff
   */

  private case class Coordinator(compensations: List[E => F[E]] = List()) {
    def pushCompensation(c: E => F[E]): Coordinator =
      Coordinator(c :: compensations)

    def doCompensate(e: E): F[E] =
      compensations.foldLeft(Monad[F].pure(e)){ case (acc, c) =>
        acc.flatMap(c)
      }
  }

  private type CoordinatorState[X] = StateT[F, Coordinator, X]
  private type StateTransactor[X] = EitherT[CoordinatorState, E, X]

  private def compiler : self.Saga ~> StateTransactor =
    new (self.Saga ~> StateTransactor) {

      override def apply[A](fa: self.Saga[A]): StateTransactor[A] =
        EitherT{
          StateT{ (coordinator: Coordinator) =>
            fa.localTransact.flatMap {
              case Right(t) =>
                val newState = coordinator.pushCompensation(fa.localCompensate)
                Monad[F].pure(newState, Right(t))
              case Left(e) =>
                coordinator
                  .pushCompensation(fa.localCompensate)
                  .doCompensate(e).map { compensationResult =>
                  (Coordinator(), Left(compensationResult))
                }
            }
          }
        }
    }

//  type StateTransactor[X] = StateT[F, Coordinator, Either[E, X]]
//
//  def compiler : self.Saga ~> StateTransactor =
//    new (self.Saga ~> StateTransactor) {
//
//      override def apply[A](fa: self.Saga[A]): StateTransactor[A] =
//        StateT.apply { (coordinator: Coordinator) =>
//          (coordinator.state, fa) match {
//            case (OkToContinue, Saga(localTransact, localCompensate)) =>
//              localTransact.flatMap {
//                case Right(t) =>
//                  val newState = coordinator.pushCompensation(localCompensate)
//                  Monad[F].pure(newState, Right(t))
//
//                case Left(e) =>
//                  coordinator
//                    .pushCompensation(localCompensate)
//                    .doCompensate(e).map { compensationResult =>
//                    (Coordinator(Compensated(compensationResult)), Left(compensationResult))
//                  }
//              }
//
//            case (Compensated(compensationResult), _) =>
//              Monad[F].pure((coordinator, Left(compensationResult)))
//          }
//        }
//    }

}

object SagaTestExample extends App {
  import cats.effect._
  import cats.effect.IO
  import cats._
  import cats.implicits._


  case class MyErrorSaga(s: String)

  val sagaBuilder: SagaBuilder[IO, MyErrorSaga] = new SagaBuilder[IO,  MyErrorSaga]
  import sagaBuilder._

  def doStuff[T](s: String)(t: T): IO[Either[MyErrorSaga, T]] = IO{ println(s) }.map(_ => Right(t))
  def generalCompensate(s: String)(e: MyErrorSaga): IO[MyErrorSaga] =
    IO{
      val value = Random.nextInt()
      println(s"compensating $s   $value   ${e.s}");
      MyErrorSaga(e.s ++ s", $value")
    }

  def sagaProgramAllGood(n: Int): SagaMonad[Int] =
    for {
      i <- sagaBuilder.saga(doStuff(s"$n fist thing generate integer 10")(10), generalCompensate(s"$n first"))
      s <- sagaBuilder.saga(doStuff(s"$n second generate string on previous result $i")(s"Generated $i"), generalCompensate(s"$n second"))
      i2 <- sagaBuilder.saga(doStuff(s"$n third measure string result $s")(s.length), generalCompensate(s"$n third"))
    } yield i + i2

  val transactedProgram: IO[Either[MyErrorSaga, Int]] = transact(sagaProgramAllGood(1) *> sagaProgramAllGood(2) *> sagaProgramAllGood(3))


  val transactedProgramWithFailure: IO[Either[MyErrorSaga, Int]] =
    transact(
      sagaProgramAllGood(1) *>
        sagaProgramAllGood(2) *> sagaBuilder.rollback(IO(MyErrorSaga("FirstError"))) *>
        sagaProgramAllGood(3))

  transactedProgram.map(println).unsafeRunSync()

  println("#####")
  println("#####")
  println("#####")

  transactedProgramWithFailure.map(println).unsafeRunSync()


}

