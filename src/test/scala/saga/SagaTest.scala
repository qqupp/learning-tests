package saga

import org.scalatest.{Assertion, FlatSpec, Matchers}
import cats.effect._
import cats._
import cats.implicits._
import cats.effect.concurrent.Ref
import saga.monad.Saga
import saga.monad.Saga.Bind

class SagaTest extends FlatSpec with Matchers {

  behavior of "Saga"

  // doEff append t to effects
  def sagaSuccess[T](t: T, effects: Ref[IO, List[String]], errors: Ref[IO, List[String]]): Saga[IO, T, String] =
    sagaBuilder(t, effects, errors)(Right(_))

  // doEff append t to effects
  // undoEff append t to effects and append 'X' to errors
  def sagaFailure[T](
      t: T,
      failure: String,
      effects: Ref[IO, List[String]],
      errors: Ref[IO, List[String]]
  ): Saga[IO, T, String] =
    sagaBuilder(t, effects, errors)(_ => Left(failure))

  def sagaBuilder[T](t: T, effects: Ref[IO, List[String]], errors: Ref[IO, List[String]])(
      successOrFailure: T => Either[String, T]
  ): Saga[IO, T, String] =
    Saga.make {
      effects.modify { l => (l ++ List(s"doEffect $t")) -> successOrFailure(t) }
    } { e =>
      effects.update(_ ++ List(s"undoEffect $t")) >>
        errors.update(_ ++ List(s"failed at $t with errors $e")) >>
        IO(s"${e}X")
    }

  it should "work like flatmap if no Left appears in the doEffect side, ignoring the undoEffect parts" in {

    val effects: Ref[IO, List[String]] = Ref.unsafe[IO, List[String]](List())
    val errors: Ref[IO, List[String]]  = Ref.unsafe[IO, List[String]](List())

    val sagaUnderTest: Saga[IO, Char, String] =
      for {
        aNumber <- sagaSuccess(1234, effects, errors)
        aString <- sagaSuccess(s"${aNumber}STR", effects, errors)
        aChar   <- sagaSuccess(aString.charAt(0), effects, errors)
      } yield aChar

    val sagaTransaction: IO[Either[String, Char]] = sagaUnderTest.transact

    val test: IO[Assertion] =
      for {
        effectsBefore <- effects.get
        errorsBefore  <- errors.get
        result        <- sagaTransaction
        effectsAfter  <- effects.get
        errorsAfter   <- errors.get
      } yield {
        effectsBefore shouldBe List()
        errorsBefore shouldBe List()

        withClue("result is the value returned by the successful chain of doEffect in the flatmap") {
          result shouldBe Right('1')
        }
        withClue("no errors appears as 'undoEffect' is ignored") {
          errorsAfter shouldBe List()
        }
        withClue("doEffects are executed in the flatmap order") {
          effectsAfter shouldBe List("doEffect 1234", "doEffect 1234STR", "doEffect 1")
        }
      }

    test.unsafeRunSync()
  }

  it should "work like flatmap until a Left is returned, then will perform 'undoEffect' in reverse order, ignoring any other continuation" in {

    val effects: Ref[IO, List[String]] = Ref.unsafe[IO, List[String]](List())
    val errors: Ref[IO, List[String]]  = Ref.unsafe[IO, List[String]](List())

    val sagaWithCompensationUnderTest: Saga[IO, Char, String] =
      for {
        aNumber       <- sagaSuccess(1234, effects, errors)
        aString       <- sagaSuccess(s"${aNumber}STR", effects, errors)
        aChar         <- sagaSuccess(aString.charAt(0), effects, errors)
        notReachable1 <- sagaFailure("FailHere", "X", effects, errors)
        notReachable2 <- sagaSuccess(false, effects, errors)
        notReachable3 <- sagaSuccess('n', effects, errors)
      } yield notReachable3

    val sagaTransaction: IO[Either[String, Char]] = sagaWithCompensationUnderTest.transact

    val test: IO[Assertion] =
      for {
        effectsBefore <- effects.get
        errorsBefore  <- errors.get
        result        <- sagaTransaction
        effectsAfter  <- effects.get
        errorsAfter   <- errors.get
      } yield {
        effectsBefore shouldBe List()
        errorsBefore shouldBe List()

        withClue("result is the value returned by reverse chain of undoEffect in the flatmap") {
          result shouldBe Left("XXXXX")
        }

        withClue("errors are populated with 'undoEffect' in reverse order") {
          errorsAfter shouldBe List(
            "failed at FailHere with errors X",
            "failed at 1 with errors XX",
            "failed at 1234STR with errors XXX",
            "failed at 1234 with errors XXXX"
          )
        }

        withClue(
          "doEffects are executed in the flatmap order until a left is encountered and undoEffects are performed in reverse order"
        ) {
          effectsAfter shouldBe List(
            "doEffect 1234",
            "doEffect 1234STR",
            "doEffect 1",
            "doEffect FailHere",
            "undoEffect FailHere",
            "undoEffect 1",
            "undoEffect 1234STR",
            "undoEffect 1234"
          )
        }
      }

    test.unsafeRunSync()
  }

  it should "work with rollback " in {
    val effects: Ref[IO, List[String]] = Ref.unsafe[IO, List[String]](List())

    val doEff                    = effects.update(_ ++ List("do")).map(Right(_))
    def undoEff(failure: String)(l: List[String]) = effects.update(_ ++ List("undo")) >> IO(l ++ List(failure))

    val saga: Saga[IO, Unit, List[String]] = for {
      u1 <- Saga.make[IO, Unit, List[String]](doEff)(undoEff("error1"))
      u2 <- Saga.make[IO, Unit, List[String]](doEff)(undoEff("error2"))
      u3 <- Saga.rollback[IO, List[String]](List("errorPure"))
      u4 <- Saga.make[IO, Unit, List[String]](doEff)(undoEff("error3"))
    } yield ()

    saga.transact.unsafeRunSync() shouldBe Left(List("errorPure", "error2", "error1"))
  }

  it should "work with nested sagas" in {
    val counter: Ref[IO, Long] = Ref.unsafe[IO, Long](0)

    def genBinaryTree(level: Int, acc: Saga[IO, Unit, Unit]): Saga[IO, Unit, Unit] =
      if (level == 0) acc else {
        genBinaryTree(level -1, Bind(acc, (_: Unit) => acc))
      }

    val saga = genBinaryTree(5, Saga.make[IO, Unit, Unit](counter.update(_ + 1).map(Right(_)))((u: Unit) => IO.unit))

    val transaction: IO[Either[Unit, Unit]] = saga.transact

    val test = for {
      countBefore <- counter.get
      _ <- transaction
      countAfter <- counter.get
    } yield {
      countAfter shouldBe Math.pow(2, 25)
    }

    test.unsafeRunSync()
  }

  it should "list tailrec flatmap" in {


    def genLeft(level: Int, acc: List[Int]): List[Int] =
      if (level == 0) acc else {
        genLeft(level -1, Monad[List].flatMap(acc)(i => List(1)))
      }


    val r = for {
      i <- genLeft(100000, List(1))
    } yield
      i

    r shouldBe List(1)
  }

  it should "work with deep (left) nested structures" in {
    var counter: Long = 0

    def incrementID: List[Unit] = Monad[List].pure(()).flatMap(_ => List({counter += 1}))

    val increment: Saga[List, Unit, Unit] = Saga.fromEffect[List, Unit, Unit](incrementID.map(Right(_)))

    def genLeft(level: Int, acc: Saga[List, Unit, Unit]): Saga[List, Unit, Unit] =
      if (level == 0) acc else {
        genLeft(level -1, Bind(acc, (_: Unit) => increment))
      }

    val saga = genLeft(100000, Saga.pure(()))

    var transaction: List[Either[Unit, Unit]] = List()
    try {
      transaction = saga.transact
    }
    catch {
      case e: Throwable =>
        println(e)
        1 shouldBe 2
    }

    val test: List[Assertion] = for {
      _ <- transaction
    } yield {
      counter shouldBe 100000
    }

  }


  it should "work with deep (right) nested structures" in {
    val counter: Ref[IO, Long] = Ref.unsafe[IO, Long](0)

    val increment: Saga[IO, Unit, Unit] = Saga.fromEffect[IO, Unit, Unit](counter.update(_ + 1).map(Right(_)))

    def genRight(level: Int, acc: Saga[IO, Unit, Unit]): Saga[IO, Unit, Unit] =
      if (level == 0) acc else {
        genRight(level -1, Bind(increment, (_: Unit) => acc))
      }

    val saga = genRight(100000, Saga.pure(()))

    val transaction: IO[Either[Unit, Unit]] = saga.transact

    val test = for {
      countBefore <- counter.get
      _ <- transaction
      countAfter <- counter.get
    } yield {
      countAfter shouldBe 100000
    }

    test.unsafeRunSync()
  }

}
