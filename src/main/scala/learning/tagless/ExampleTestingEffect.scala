package learning.tagless
import cats._
import cats.data._
import cats.implicits._
import cats.effect.Sync
import learning.tagless.ExampleTestingEffect.te

object ExampleTestingEffect extends App {

  trait MyAlgebra[F[_]] {
    def doStuff(a: Int): F[Unit]
  }

  def myProg[F[_]: Sync](myAlgebra: MyAlgebra[F]): F[Unit] =
    for {
      _ <- myAlgebra.doStuff(1)
      _ <- myAlgebra.doStuff(2)
      _ <- myAlgebra.doStuff(3)
    } yield ()


  case class TestEnv(countCalls: Int, acc: Int)

  val te = new TestingEffect[TestEnv] ()
  import te.TestEffect
  implicit val syncTestEffect = te.Sync.instance


  val myAlgebraTestInterpreter: MyAlgebra[TestEffect] = new MyAlgebra[TestEffect] {

    //every time call doStuff increment countCalls and add up a to acc in TestEnv
    def doStuff(a: Int): TestEffect[Unit] =
      EitherT {
        State { s =>
          val newState: TestEnv = s.copy(countCalls = s.countCalls + 1, acc = s.acc + a)
          (newState, Right(()))
        }
      }
  }

  val result = myProg[TestEffect](myAlgebraTestInterpreter)

  println(result.value.runS(TestEnv(0, 0)).value)


}
