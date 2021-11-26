sealed abstract case class Fun[T1, R](f: T1 => R) extends (T1 => R) {
  def apply(v1: T1): R = f(v1)

  override def andThen[A](g: R => A): T1 => A = CombinedFun(f, g)

  override def compose[A](g: A => T1): A => R = CombinedFun(g, f)

  override def toString(): String = "Function1Wrapper"
}

object Fun {
  def apply[A, B](f: A => B): A => B =
    f match {
      case Fun(f)                   => f
      case cf: CombinedFun[_, _, _] => cf
      case _                        => new Fun(f) {}
    }
}

case class CombinedFun[T1, T2, T3](f1: T1 => T2, f2: T2 => T3) extends (T1 => T3) {
  def apply(v1: T1): T3 = {
    def go(acc: Any, stack: List[Any => Any]): T3 =
      stack match {
        case Nil                         => acc.asInstanceOf[T3]
        case CombinedFun(ff, gg) :: rest => go(acc, w(ff) :: w(gg) :: rest)
        case ff :: rest                  => go(ff(acc), rest)
      }

    go(v1, w(f1) :: w(f2) :: Nil)
  }

  override def andThen[A](g: T3 => A): T1 => A = CombinedFun(this, g)

  override def compose[A](g: A => T1): A => T3 = CombinedFun(g, this)

  private final def w(f: _ => _) = f.asInstanceOf[Any => Any]

  override def toString(): String = "CombinedFunction"

}
