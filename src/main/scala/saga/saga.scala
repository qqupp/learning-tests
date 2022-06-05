package saga

import cats.{Monad, _}
import cats.implicits._

sealed trait Saga[F[_], +A, +E]

object Saga {

  def make[F[_], A, E](
      doEffect: F[Either[E, A]]
  )(
      undoEffect: E => F[E]
  ): Saga[F, A, E] =
    Eff(doEffect, undoEffect)

  def rollback[F[_], E](e: E): Saga[F, Nothing, E] =
    Failure(e)

  def transact[F[_]: Monad, A, E](saga: Saga[F, A, E]): F[Either[E, A]] =
    transact_(saga)

  // combinators
  def pure[F[_]: Applicative, A, E](a: A): Saga[F, A, E] = {
    val pv: Either[E, A] = Either.right[E, A](a)
    Eff(Applicative[F].pure(pv), (e: E) => Applicative[F].pure(e))
  }

  def flatMap[F[_]: Applicative, A, E, B, EE >: E](saga: Saga[F, A, E])(f: A => Saga[F, B, EE]): Saga[F, B, EE] =
    saga match {
      case Failure(e) => Failure(e)
      case _          => Bind(saga, f)
    }

  def map[F[_]: Applicative, A, E, B, EE >: E](saga: Saga[F, A, E])(f: A => B): Saga[F, B, EE] =
    flatMap(saga)(a => pure(f(a)))

  // syntax
  implicit class SagaSyntax[F[_]: Monad, +A, +E](saga: Saga[F, A, E]) {
    def map[EE >: E, B](f: A => B): Saga[F, B, EE]                  = Saga.map(saga)(f)
    def flatMap[EE >: E, B](f: A => Saga[F, B, EE]): Saga[F, B, EE] = Saga.flatMap[F, A, E, B, EE](saga)(f)
    def transact[AA >: A, EE >: E]: F[Either[EE, AA]]               = Saga.transact(saga)
  }

  // private interface
  private case class Failure[F[_], A, E](e: E)                                                    extends Saga[F, A, E]
  private case class Eff[F[_], A, E](doEff: F[Either[E, A]], undoEff: E => F[E])                  extends Saga[F, A, E]
  private case class Bind[F[_], A, E, EE >: E, B](saga: Saga[F, A, E], cont: A => Saga[F, B, EE]) extends Saga[F, B, EE]

  private def transact_[F[_]: Monad, A, E](saga: Saga[F, A, E]): F[Either[E, A]] = {
    val M = implicitly[Monad[F]]

    def anyfy[X, Y, Z](f: X => Saga[F, Y, Z]) = f.asInstanceOf[Any => Saga[F, Any, Any]]

    def recover(eff: F[Either[Any, Any]], handles: List[Any => F[Any]]): F[Either[Any, Any]] = {
      if (handles.isEmpty) eff
      else
        for {
          errorOrResult   <- eff
          resultRecovered <- errorOrResult match {
                               case result @ Right(_) => M.pure(result)
                               case Left(error)       =>
                                 val h :: hs = handles
                                 hs.foldLeft(h(error)) {
                                     case (acc, handle) =>
                                       acc.flatMap(handle)
                                   }
                                   .map(Left(_))
                             }
        } yield resultRecovered
    }

    def gooo(
        rollback: List[Any => F[Any]],
        current: Saga[F, Any, Any],
        cont: List[Any => Saga[F, Any, Any]]
    ): F[Either[E, A]] =
      current match {
        case Eff(doEff, undoEff) =>
          cont match {
            case Nil     => recover(doEff, undoEff :: rollback).asInstanceOf[F[Either[E, A]]]
            case k :: ks =>
              doEff.flatMap {
                case Right(x) => gooo(undoEff :: rollback, k(x), ks)
                case Left(e)  => recover(M.pure(Left(e)), undoEff :: rollback).asInstanceOf[F[Either[E, A]]]
              }
          }
        case Failure(e)          =>
          recover(M.pure(Left(e)), rollback).asInstanceOf[F[Either[E, A]]]
        case Bind(s1, k1)        =>
          gooo(rollback, s1, anyfy(k1) :: cont)
      }

//    def go(
//        rollback: List[Any => F[Any]],
//        current: F[Saga[F, Any, Any]],
//        cont: List[Any => Saga[F, Any, Any]]
//    ): F[Either[E, A]] =
//      current.flatMap {
//        case Eff(doEff, undoEff) =>
//          cont match {
//            case Nil     => recover(doEff, undoEff :: rollback).asInstanceOf[F[Either[E, A]]]
//            case k :: ks =>
//              val ret: F[Saga[F, Any, Any]] = doEff.map {
//                case Right(x) => k(x)
//                case Left(e)  => Failure[F, Any, Any](e)
//              }
//              go(undoEff :: rollback, ret, ks)
//          }
//        case Failure(e)          =>
//          recover(M.pure(Left(e)), rollback).asInstanceOf[F[Either[E, A]]]
//        case Bind(s1, k1)        =>
//          go(rollback, M.pure(s1), anyfy(k1) :: cont)
//      }

    gooo(List(), saga, List())

  }

}
