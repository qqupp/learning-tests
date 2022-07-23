package atleastonetype

/*
  A heterogenus list that may or may not contain elements of all declared types

  a model for AtLeastOneNonEmpty(a: Option[A], b: Option[B] ....)

 */

// requires scala 2.13
sealed trait HOptionalList[+H, +T]

object HOptionalList {
  //Alias for conventience
  type End       = HOptionalList[Nothing, Nothing]
  type ++:[H, T] = AtLeast1[H, T]
  type +:[H, T]  = HOptionalList[H, T]

  // A HList with optional values
  sealed trait AtLeast0[+H, +T] extends HOptionalList[H, T]

  case object HONil                                                extends AtLeast0[Nothing, Nothing]
  case class NotThis[H, HH, TT](other: AtLeast0[HH, TT])           extends AtLeast0[H, HOptionalList[HH, TT]]
  case class ThisAnd[H, HH, TT](value: H, other: AtLeast0[HH, TT]) extends AtLeast0[H, HOptionalList[HH, TT]]

  // this type guarantee that at least one of the types is not empty
  sealed trait AtLeast1[+H, +T] extends AtLeast0[H, T]

  case class ThisOneAnd[H, HH, TT](value: H, other: HOptionalList[HH, TT]) extends AtLeast1[H, HOptionalList[HH, TT]]
  case class NotThisOne[H, HH, TT](other: AtLeast1[HH, TT])                extends AtLeast1[H, HOptionalList[HH, TT]]

  def getFirst[X]: GetPartiallyApplied[X] = new GetPartiallyApplied[X]

  class GetPartiallyApplied[X] {
    def apply[H, T](atLeast: HOptionalList[H, T])(implicit ev: Getter[HOptionalList[H, T], X]): Option[X] =
      ev.get(atLeast)
  }

  trait Getter[A, B] {
    def get(a: A): Option[B]
  }

  implicit def baseGetter[H, T]: Getter[HOptionalList[H, T], H] =
    new Getter[HOptionalList[H, T], H] {
      def get(a: HOptionalList[H, T]): Option[H] =
        a match {
          case v: ThisOneAnd[H, _, _] => Some(v.value)
          case v: NotThisOne[H, _, _] => None
          case HONil                  => None
          case v: NotThis[H, _, _]    => None
          case v: ThisAnd[H, _, _]    => Some(v.value)
        }
    }

  implicit def inductiveGetter[HH, H, T, X](implicit
      ev: Getter[HOptionalList[H, T], X]
  ): Getter[HOptionalList[HH, HOptionalList[H, T]], X] =
    new Getter[HOptionalList[HH, HOptionalList[H, T]], X] {
      def get(a: HOptionalList[HH, HOptionalList[H, T]]): Option[X] =
        a match {
          case v: ThisOneAnd[HH, H, T] => ev.get(v.other)
          case v: NotThisOne[HH, H, T] => ev.get(v.other)
          case HONil                   => None
          case v: NotThis[HH, H, T]    => ev.get(v.other)
          case v: ThisAnd[HH, H, T]    => ev.get(v.other)
        }
    }

  def updateFirst[A]: PartiallyAppliedUpdater[A] = new PartiallyAppliedUpdater[A]

  class PartiallyAppliedUpdater[A] {
    def apply[H, T](list: HOptionalList[H, T], f: Option[A] => A)(implicit
        ev: Updater[HOptionalList[H, T], A, AtLeast1[H, T]]
    ): AtLeast1[H, T] =
      ev.update(list, f)
  }

  // transforms AtLeast0 to AtLeast1
  trait Updater[A, B, C] {
    def update(a: A, f: Option[B] => B): C
  }

  implicit def baseUpdater[HH, H, T]: Updater[
    HOptionalList[HH, HOptionalList[H, T]],
    HH,
    AtLeast1[HH, HOptionalList[H, T]]
  ] =
    new Updater[HOptionalList[HH, HOptionalList[H, T]], HH, AtLeast1[HH, HOptionalList[H, T]]] {
      def update(a: HOptionalList[HH, HOptionalList[H, T]], f: Option[HH] => HH): AtLeast1[HH, HOptionalList[H, T]] =
        a match {
          case v: ThisOneAnd[HH, H, T] =>
            ThisOneAnd(f(Some(v.value)), v.other)
          case v: NotThisOne[HH, H, T] => ThisOneAnd(f(None), v.other)
          case HONil                   => ThisOneAnd(f(None), HONil)
          case v: NotThis[HH, H, T]    => ThisOneAnd(f(None), v.other)
          case v: ThisAnd[HH, H, T]    =>
            ThisOneAnd(f(Some(v.value)), v.other)
        }
    }

  implicit def inductiveUpdater[X, HH, HA, TA, HB, TB](implicit
      ev: Updater[HOptionalList[HA, TA], X, AtLeast1[HB, TB]]
  ): Updater[
    HOptionalList[HH, HOptionalList[HA, TA]],
    X,
    AtLeast1[HH, HOptionalList[HB, TB]]
  ] =
    new Updater[HOptionalList[HH, HOptionalList[HA, TA]], X, AtLeast1[HH, HOptionalList[HB, TB]]] {
      def update(a: HOptionalList[HH, HOptionalList[HA, TA]], f: Option[X] => X): AtLeast1[HH, HOptionalList[HB, TB]] =
        a match {
          case v: ThisOneAnd[HH, HA, TA] => ThisOneAnd(v.value, ev.update(v.other, f))
          case v: NotThisOne[HH, HA, TA] => NotThisOne(ev.update(v.other, f))
          case HONil                     =>
            NotThisOne(ev.update(HONil, f))
          case v: NotThis[HH, HA, TA]    => NotThisOne(ev.update(v.other, f))
          case v: ThisAnd[HH, HA, TA]    => ThisOneAnd(v.value, ev.update(v.other, f))
        }
    }

}



object Examples extends App {
  import HOptionalList._

  type TestType = Boolean ++: Int +: String +: Double +: End

  val t3: Boolean +: Int +: String +: Double +: End =
    NotThisOne(ThisOneAnd(1, HONil))


  val t1: TestType =
    ThisOneAnd(false, HONil)



  val fullHouse: TestType =
    ThisOneAnd(false, ThisAnd(10, ThisAnd("test", ThisAnd(6.3, HONil))))

  val almostFull: TestType =
    ThisOneAnd(false, NotThis(ThisAnd("test", ThisAnd(6.3, HONil))))


  def countElements[H, T](l: HOptionalList[H, T]): Int = l match {
    case HONil => 0
    case NotThis(other) => countElements(other)
    case ThisAnd(_, other) => 1 + countElements(other)
    case NotThisOne(other) => countElements(other)
    case ThisOneAnd(_, other) => 1 + countElements(other)
  }

  println(fullHouse)
  println(countElements(fullHouse))
  println(almostFull)
  println(countElements(almostFull))

  println("******* AlmostFull Test")
  println(almostFull)
  val r0 = getFirst[Boolean](almostFull)
  val r1 = getFirst[Int](almostFull)
  val r2 = getFirst[String](almostFull)
  val r3 = getFirst[Double](almostFull)

  println(r0)
  println(r1)
  println(r2)
  println(r3)

  println("**** updateTest")
  val u0: TestType = updateFirst[Boolean](almostFull, _ => true)
  val before0 = getFirst[Boolean](almostFull)
  val after0 =  getFirst[Boolean](u0)
  println("------------")
  println("update boolean")
  println(u0)
  println(before0)
  println(after0)
  println("------------")

  val u1: TestType = updateFirst[Int](almostFull, _ => 5674)
  val before1 = getFirst[Int](almostFull)
  val after1 =  getFirst[Int](u1)
  println("------------")
  println("update Int")
  println(u1)
  println(before1)
  println(after1)
  println("------------")

  val u2: TestType = updateFirst[String](almostFull, _ => "new value here")
  val before2 = getFirst[String](almostFull)
  val after2 =  getFirst[String](u2)
  println("------------")
  println("update String")
  println(u2)
  println(before2)
  println(after2)
  println("------------")


  val u3: TestType = updateFirst[Double](almostFull, _ => 3.14)
  val before3 = getFirst[Double](almostFull)
  val after3 =  getFirst[Double](u3)
  println("------------")
  println("update Double")
  println(u3)
  println(before3)
  println(after3)
  println("------------")


  println("Test long chain empty")
  type TestChain = Char ++: Int +: Long +: Double +: String +: End

  val longchain: TestChain = ThisOneAnd('c', HONil)
  val updatedLast: TestChain = updateFirst[String](longchain, _ => "ohohohoh")
  println("------------")
  println(longchain)
  println(updatedLast)
  println("------------")





}