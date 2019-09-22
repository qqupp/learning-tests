package learning
import org.scalacheck.Arbitrary

object ApplicativeArbitrary extends App {

  implicit def applicative[A, B](implicit arbA: Arbitrary[A], arbF: Arbitrary[A => B]): Arbitrary[B] =
    Arbitrary {
      for {
        a <- arbA.arbitrary
        f <- arbF.arbitrary
      } yield f(a)
    }

  case class AA(i: Int, s: String, c: Int)

  val as: Int => String => Int => AA = AA.apply _ curried

  def genArb[A, B](g: A => B)(implicit arbA: Arbitrary[A]): Arbitrary[B] = {
     Arbitrary{
       for {
         l <- arbA.arbitrary
       } yield g(l)
     }
   }

  implicit val aaaaa: Arbitrary[String => Int => AA] = genArb(as)

  implicitly[Arbitrary[AA]]

}
