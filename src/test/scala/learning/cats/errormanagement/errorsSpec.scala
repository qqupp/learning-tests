package learning.cats.errormanagement

import java.util.Date

import cats._
import cats.data._
import cats.implicits._
import cats.effect.IO
import org.scalatest.{FlatSpec, Matchers}

class errorsSpecErrorInIOChannel extends FlatSpec with Matchers {

  case object WrongUserName extends RuntimeException("some descr user")
  case object WrongPassword extends RuntimeException("some descr passwd")
  case class ExpiredUser(date: Date) extends RuntimeException("some descr date")

  trait User
  trait Module {
    def findUserByName(user: String): IO[User]
    def checkPassword(user: User, pswd: String): IO[Unit]
    def checkSubscription(user: User): IO[Unit]
    def checkUserStatus(user: User): IO[Unit]
  }

  def auth(m: Module)(username: String, pass: String): IO[User] =
    (for {
      user <- m.findUserByName(username)
      _ <- m.checkPassword(user, pass)
      _ <- m.checkSubscription(user)
      _ <- m.checkUserStatus(user)
    } yield user
      ).recoverWith{
      case WrongPassword => IO{???}
      case WrongUserName => IO{???}
      case ExpiredUser(d) => IO{???}
    }


}



class errorsSpecErrorInsideIO extends FlatSpec with Matchers {

  trait AuthError
  case object WrongUserName extends AuthError
  case object WrongPassword extends AuthError
  case class ExpiredUser(date: Date) extends AuthError
  case class WrongStatus() extends AuthError

  trait User
  trait Module {
    def findUserByName(user: String): IO[Either[WrongUserName.type , User]]
    def checkPassword(user: User, pswd: String): IO[Either[WrongPassword.type ,Unit]]
    def checkSubscription(user: User): IO[Either[ExpiredUser,Unit]]
    def checkUserStatus(user: User): IO[Either[WrongStatus, Unit]]
  }

  def auth(m: Module)(username: String, pass: String): IO[Either[AuthError,User]] =
    (for {
      user <-  EitherT(m.findUserByName(username))
      _ <- EitherT(m.checkPassword(user, pass))
      _ <- EitherT(m.checkSubscription(user))
      _ <- EitherT(m.checkUserStatus(user))
    } yield user
      ).value


}
