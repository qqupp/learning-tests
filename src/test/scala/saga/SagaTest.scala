package saga

import org.scalatest.{Assertion, FlatSpec, Matchers}
import cats.effect._
import cats._
import cats.implicits._
import Saga._
import cats.effect.concurrent.Ref

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

}
