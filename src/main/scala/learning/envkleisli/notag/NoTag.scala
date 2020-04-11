package learning.envkleisli.notag

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

trait Database[F[_]] {
  def get(k: String): F[Option[String]]
}

trait Cache[F[_]] {
  def get(k: String): F[Option[String]]
  def save(s: String): F[String]
}

object Program {

  def progLogic[F[_] : Monad](user: String)(cache: Cache[F], database: Database[F]): F[String] =
    for {
      userKOpt <- database.get(user)
      o: Option[String] = userKOpt
      cacheOpt <- o match {
        case Some(s) => cache.get(s)
        case None => Monad[F].pure[Option[String]](None)
      }
    } yield cacheOpt.fold("unkn")(x => s"you are $x")

}

object ModelImp1 {

  val database = new Database[IO] {
    def get(k: String): IO[Option[String]] = IO{ println(s"request database $k"); Some(k)}
  }

  val cache = new Cache[IO] {
    def get(k: String): IO[Option[String]] = IO{ println(s"request cache $k"); Some("1234")}

    def save(s: String): IO[String] = IO{ println(s"saving cache $s"); "1232" }
  }

}


object ProgRun1 extends App {

  import Program._
  import ModelImp1._


  val prog = progLogic[IO]("paolo")(cache, database)

  prog.unsafeRunSync()

}

case class RuntimeValue(s: String)
trait RuntimeEnv[F[_]] {
  def get: F[RuntimeValue]
}

object ModelImp2 {


  val runtime = new RuntimeEnv[IO] {
    def get: IO[RuntimeValue] = IO(RuntimeValue("this is the env!"))
  }

  val database = new Database[IO] {
    def get(k: String): IO[Option[String]] = IO{ println(s"request database $k"); Some(k)}
  }


  type Env[T] = Kleisli[IO, RuntimeValue,  T]

  val cacheEnv = new Cache[Env] {
    def get(k: String): Env[Option[String]] = Kleisli{ e =>
      IO{
        println(s" cache request from env $e")
        println(s"request cache $k")
        Some("1234")
      }
    }

    def save(s: String): Env[String] = Kleisli.liftF(IO{ println(s"saving cache $s"); "1232" })
  }

}



object ProgRun2 extends App {

  import Program._
  import ModelImp2._

  val databaseEnv = new Database[Env] {
    def get(k: String): Env[Option[String]] = Kleisli.liftF(database.get(k))
  }

  val fetchEnv: IO[RuntimeValue] =  runtime.get
  val prog: Env[String] = progLogic[Env]("paolo")(cacheEnv, databaseEnv)


  fetchEnv.flatMap(env => prog.run(env)).unsafeRunSync()

}
