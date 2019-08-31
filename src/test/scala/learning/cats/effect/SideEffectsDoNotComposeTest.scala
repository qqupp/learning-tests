package learning.cats.effect

import cats._
import cats.data.Kleisli
import cats.implicits._
import cats.syntax._
import cats.effect.IO
import com.sun.xml.internal.bind.v2.schemagen.episode.Klass
import org.scalatest.{FlatSpec, Matchers}

class SideEffectsDoNotComposeTest extends FlatSpec with Matchers {

  "Impure function" should "not compose" in new StdOutMockContext {

    def compose[A, B, C](f: A => B, g: B => C): A => C = x => g( f (x) )

    def impureSquare(x: Int): Int = { StdOut.print(s"input $x result ${x*x}"); x * x }

    val aList = List(2,3)

    StdOut.clean()
    val squareTwiceList =  aList.map( impureSquare ).map( impureSquare )
    val expectedResult =  List(16,81)
    val expectedSideEffect = List("input 2 result 4", "input 3 result 9", "input 4 result 16", "input 9 result 81")

    squareTwiceList shouldBe expectedResult
    StdOut.show() shouldBe expectedSideEffect


    StdOut.clean()
    val squareTwiceListRefactoring =  aList.map( compose(impureSquare, impureSquare ) )
    val expectedResultRefactoring =  List(16,81)
    val expectedSideEffectRefactoring = List("input 2 result 4", "input 4 result 16", "input 3 result 9",  "input 9 result 81")

    squareTwiceListRefactoring shouldBe expectedResultRefactoring
    StdOut.show() shouldBe expectedSideEffectRefactoring


    // after refactoring we get the same result but the side effects are different!
    expectedResult shouldBe expectedResultRefactoring
    expectedSideEffect shouldNot be(expectedSideEffectRefactoring)

  }


  "Pure function" should "compose" in new StdOutMockContext {

    def compose[A, B, C](f: A => IO[B], g: B => IO[C]): A => IO[C] = x => f(x).flatMap(g)

    def pureSquare(x: Int): IO[Int] = IO{ StdOut.print(s"input $x result ${x*x}"); x * x }

    val aList = List(2,3)

    StdOut.clean()
    val squareTwiceList: IO[List[Int]] =  aList.map( pureSquare ).map(io => io.flatMap(pureSquare) ).sequence

    val result = squareTwiceList.unsafeRunSync()
    val expectedResult =  List(16,81)
    val expectedSideEffect = List("input 2 result 4", "input 4 result 16",  "input 3 result 9", "input 9 result 81")

    result shouldBe expectedResult
    StdOut.show() shouldBe expectedSideEffect


    StdOut.clean()
    val squareTwiceListRefactoring: IO[List[Int]]  =  aList.map( compose(pureSquare, pureSquare ) ).sequence

    val resultRefactoring = squareTwiceListRefactoring.unsafeRunSync()
    val expectedResultRefactoring =  List(16,81)
    val expectedSideEffectRefactoring = List("input 2 result 4", "input 4 result 16",  "input 3 result 9", "input 9 result 81")

    resultRefactoring shouldBe expectedResultRefactoring
    StdOut.show() shouldBe expectedSideEffectRefactoring


    // after refactoring we get the same result but the side effects are different!
    expectedResult shouldBe expectedResultRefactoring
    expectedSideEffect shouldBe expectedSideEffectRefactoring

  }


  trait StdOutMockContext {

    object StdOut {
      private var out: List[String] = List()

      def show(): List[String] = out
      def print[T](s: T): Unit = out = out ++ List(s.toString)
      def clean(): Unit = out = List()
    }

  }
}
