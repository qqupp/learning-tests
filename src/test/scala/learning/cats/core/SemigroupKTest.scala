package learning.cats.core

import cats.SemigroupK
import org.scalatest.{FlatSpec, Matchers}

class SemigroupKTest extends FlatSpec with Matchers {

  "A semigourpK" should "combinek any 2 F[_] with combineK" in {
    import cats.instances.list._
    SemigroupK[List].combineK(List(1,2,3), List(4,5)) shouldBe List(1,2,3,4,5)

    //this is not very obvious but I guess you can implement it in different ways?
    import cats.instances.option._
    SemigroupK[Option].combineK(Some(10), Some(22)) shouldBe Some(10)
  }

  it should "allow for <+> syntax" in {
    import cats.syntax.semigroupk._
    import cats.instances.list._

    List(1,3) <+> List(7,8) shouldBe List(1,3,7,8)
  }

  it should "use algebra" in {
    import cats.kernel.Semigroup
    import cats.instances.list
    val fromSemigroupK: Semigroup[List[Int]] = SemigroupK.apply[List](list.catsStdInstancesForList).algebra[Int]
    val fromCats: Semigroup[List[Int]]       = Semigroup.apply[List[Int]](list.catsKernelStdMonoidForList[Int])

    val l1 = List(1)
    val l2 = List(2)
    fromSemigroupK.combine(l1, l2) shouldBe fromCats.combine(l1, l2)
  }

  it should "compose F and G" in {
    case class TestF[T](t: T)
    type TestType[T] = List[TestF[T]]

    import cats.instances.list._
    val semigourpkForTestType: SemigroupK[TestType] = SemigroupK[List].compose[TestF]


    semigourpkForTestType.combineK( List(TestF(1)), List(TestF(2))) shouldBe List(TestF(1), TestF(2))
  }
}
