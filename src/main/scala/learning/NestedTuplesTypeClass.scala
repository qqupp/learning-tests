package learning

object NestedTuplesTypeClass extends App {

  implicit class TupleOps[P <: Product](p: P) {
    def ::[A](a: A): (A, P) = (a, p)
  }

  // to terminate the tuple list
  sealed trait PNil
  case object PNil extends PNil with Product

  sealed trait Direction
  case object Found                 extends Direction
  case class Left(from: Direction)  extends Direction
  case class Right(from: Direction) extends Direction

  trait AhasB[A, B, D <: Direction] {
    def dir: Direction
    def from(a: A): B
  }

  implicit def tupleBaseLeft[A, B]: AhasB[Tuple2[A, B], A, Left] =
    new AhasB[Tuple2[A, B], A, Left] {
      def dir: Direction     = Left(Found)
      def from(a: (A, B)): A = a._1
    }

  implicit def tupleBaseRight[A, B]: AhasB[Tuple2[A, B], B, Right] =
    new AhasB[Tuple2[A, B], B, Right] {
      def dir: Direction     = Right(Found)
      def from(a: (A, B)): B = a._2
    }

  implicit def tupleIndR[A, B, T, D <: Direction](implicit bHasT: AhasB[B, T, D]): AhasB[Tuple2[A, B], T, Right] =
    new AhasB[Tuple2[A, B], T, Right] {
      def dir: Direction     = Right(bHasT.dir)
      def from(a: (A, B)): T = bHasT.from(a._2)
    }

  implicit def tupleIndLL[A, B, T, D <: Direction](implicit bHasT: AhasB[A, T, D]): AhasB[Tuple2[A, B], T, Left] =
    new AhasB[Tuple2[A, B], T, Left] {
      def dir: Direction     = Left(bHasT.dir)
      def from(a: (A, B)): T = bHasT.from(a._1)
    }

  class PartiallyAppliedHas[B] {
    def apply[A](a: A)(implicit ev: AhasB[A, B, _]): B = ev.from(a)
  }
  def has[B] = new PartiallyAppliedHas[B] {}

  /// Examples

  case class AA(i: Int)

  trait Num {
    val n: Int
  }

  trait Den {
    val d: Int
  }

  val myEnv: (String, (Int, (AA, (Char, (Num with Den, (Double, PNil.type)))))) =
    "one" :: 2 :: AA(3) :: '4' :: new Num with Den { val n: Int = 10; val d: Int = 20 } :: 6.0 :: PNil

  val r01                                                                       = has[String](myEnv)
  val r02                                                                       = has[Int](myEnv)
  val r03                                                                       = has[AA](myEnv)
  val r04                                                                       = has[Char](myEnv)
  val r05                                                                       = has[Num with Den](myEnv)
  val r06                                                                       = has[Double](myEnv)

  println(r01)
  println(r02)
  println(r03)
  println(r04)
  println(r05)
  println(r06)

  // deep nested test
  case class T01()
  case class T02()
  case class T03()
  case class T04()
  case class T05()
  case class T06()
  case class T07()
  case class T08()
  case class T09()
  case class T10()
  case class T11()
  case class T12()
  case class T13()
  case class T14()
  case class T15()
  case class T16()

  type TestType =
    ((((T01, T02), (T03, T04)), ((T05, T06), (T07, T08))), (((T09, T10), (T11, T12)), ((T13, T14), (T15, T16))))

  val test: TestType = (
    (
      (
        (
          (
            (
              (
                T01(),
                T02()
              )
            ),
            (
              (
                T03(),
                T04()
              )
            )
          )
        ),
        (
          (
            (
              (
                T05(),
                T06()
              )
            ),
            (
              (
                T07(),
                T08()
              )
            )
          )
        )
      )
    ),
    (
      (
        (
          (
            (
              (
                T09(),
                T10()
              )
            ),
            (
              (
                T11(),
                T12()
              )
            )
          )
        ),
        (
          (
            (
              (
                T13(),
                T14()
              )
            ),
            (
              (
                T15(),
                T16()
              )
            )
          )
        )
      )
    )
  )

  val testPath = has[T06](test)

  println(testPath)

  println(implicitly[AhasB[TestType, T06, _]].dir)

}
