package learning.envkleisli.tag


/*
  Hey guys I've got plenty of spare time so that I was investigating a bit some approaches on code extensibility,
  If you have 10 minutes spare you may want to have a look at this, so maybe can discuss it in depth and compare our
  solutions in the next scalaX??

  This is a toy example, the logic of the program it is not important,
  there are
    - 2 interfaces for communicating with cache and database
    - a simple program logic implemented
    - a "production ready" implementation for the interfaces.
    - the

  The aim of this exercise is to extend the functionality
  We want to printout a value available a runtime every time we get data from the cache 'Cache.get'
  That may be a real production use case, like logging trace id from big brother or similar.

  an interface to retrieve the runtime value may look like this
  case class RuntimeValue(s: String)
  trait RuntimeEnv {
    def get: IO[RuntimeValue]
  }


  no restriction in general, but we want to keep the program logic and the interfaces as much as possible untouched,
  this is an implementation detail!


  Have fun and Happy Easter!


 */
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

//pure interfaces
trait Database {
  def get(k: String): IO[Option[String]]
}

trait Cache {
  def get(k: String): IO[Option[String]]
  def save(s: String): IO[String]
}

// program logic
object Program {

  def progLogic(user: String)(cache: Cache, database: Database): IO[String] =
    for {
      userKOpt <- database.get(user)
      o: Option[String] = userKOpt
      cacheOpt <- o match {
        case Some(s) => cache.get(s)
        case None => IO(None)
      }
    } yield cacheOpt.fold("unkn")(x => s"you are $x")

}

// interface implementations
object ModelImp1 {

  val database = new Database {
    def get(k: String): IO[Option[String]] = IO{ println(s"request database $k"); Some("A")}
  }

  val cache = new Cache {
    def get(k: String): IO[Option[String]] = IO{ println(s"request cache $k"); Some("B")}

    def save(s: String): IO[String] = IO{ println(s"saving cache $s"); "C" }
  }

}

// program invocation
object ProgRun1 extends App {

  import Program._
  import ModelImp1._

  val prog = progLogic("D")(cache, database)

  prog.unsafeRunSync()

}


case class RuntimeValue(s: String)
trait RuntimeEnv {
  def get: IO[RuntimeValue]
}



object ModelImp2 {

  val database = new Database {
    def get(k: String): IO[Option[String]] = IO{ println(s"request database $k"); Some(k)}
  }


  val runtimeEnv = new RuntimeEnv {
    def get: IO[RuntimeValue] = IO(RuntimeValue("a runtime value"))
  }

  val cache = new Cache {
    def get(k: String): IO[Option[String]] =
      IO{
        println(s"request cache $k")
        Some("1234")
      }

    def save(s: String): IO[String] = IO{ println(s"saving cache $s"); "1232" }
  }

}


// program invocation
object ProgRun2 extends App {

  import Program._
  import ModelImp2._

   val runtimeEnv = new RuntimeEnv {
    def get: IO[RuntimeValue] = IO(RuntimeValue("a runtime value"))
  }

  val fetch: IO[RuntimeValue] = runtimeEnv.get
  val prog: IO[String] = progLogic("D")(cache, database)


  prog.unsafeRunSync()

}

