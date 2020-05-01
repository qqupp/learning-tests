package purecommands.semantic.domain

sealed trait Eval

object Eval {

  final case class EInt(i: Int) extends Eval

  final case class EBool(b: Boolean) extends Eval

  final case object Novalue extends Eval

}
