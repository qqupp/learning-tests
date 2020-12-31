package learning

object HasTypeclass {

  trait Has[X, Y] {
    def from(x: X): List[Y]
  }

  object Has {
    implicit def baseT[T]: Has[T, T] =
      new Has[T, T] {
        def from(x: T): List[T] = List(x)
      }
  }

  class PartiallyAppliedHas[Y] {
    def apply[X](x: X)(implicit ev: X Has Y): List[Y] = ev.from(x)
  }

  def has[Y] = new PartiallyAppliedHas[Y]

  object Tuple2HasInstances extends Tuple2HasLowPriority {

    implicit def inductiveAB[A, B, Y](implicit aHasY: Has[A, Y], bHasY: Has[B, Y]): Has[Tuple2[A, B], Y] =
      new Has[Tuple2[A, B], Y] {
        def from(x: (A, B)): List[Y] = aHasY.from(x._1) ++ bHasY.from(x._2)
      }

  }

  trait Tuple2HasLowPriority {
    implicit def inductiveA[A, X, Y](implicit xHasY: Has[X, Y]): Has[Tuple2[A, X], Y] =
      new Has[Tuple2[A, X], Y] {
        def from(x: (A, X)): List[Y] = xHasY.from(x._2)
      }

    implicit def inductiveB[B, X, Y](implicit xHasY: Has[X, Y]): Has[Tuple2[X, B], Y] =
      new Has[Tuple2[X, B], Y] {
        def from(x: (X, B)): List[Y] = xHasY.from(x._1)
      }
  }

}
