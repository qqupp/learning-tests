package simulatedHigherKinds


class OptionKind[T] extends Higher[OptionKind.Tpe.type, T] {

  var underline: Option[T] = None

  def set(t: T): Unit = {
    underline = Some(t)
  }

  def get: Option[T] = underline

}

object OptionKind {
  object Tpe

  def downCast[T](hkt: Higher[OptionKind.Tpe.type, T]): Option[T] =
    hkt.asInstanceOf[OptionKind[T]].get


  def functorInstance = new FunctorTypeClass[OptionKind.Tpe.type ] {
    override def map[A, B](fa: Higher[Tpe.type, A])(f: A => B): Higher[Tpe.type, B] = {
      val ok = OptionKind.downCast(fa)
      val ob = new OptionKind[B] {}
      if (ok.isDefined) ob.set(f(ok.get))
      ob
    }
  }
}
