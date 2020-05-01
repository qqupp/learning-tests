package purecommands.semantic.domain

sealed trait Dval

object Dval {
  type Loc = Int

  final case class DInt(i :Int) extends Dval

  final case class DBool(b: Boolean) extends Dval

  final case class DLoc(l: Loc) extends Dval

  final case object Unbound extends Dval

}
