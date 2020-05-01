package purecommands.semantic.domain

sealed trait Mval

object Mval {

  final case class MInt(i: Int) extends Mval

  final case class MBool(b: Boolean) extends Mval

  final case object Undefined extends Mval

}
