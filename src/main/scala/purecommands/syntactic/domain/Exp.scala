package purecommands.syntactic.domain

sealed trait Exp

object Exp {

  type Ide = String
  final case class LitteralInt(i: Int) extends Exp
  final case class LitteralBool(b: Boolean) extends Exp
  final case class Den(d: Ide) extends Exp
  final case class Prod(e1: Exp, e2: Exp) extends Exp
  final case class Sum(e1: Exp, e2: Exp) extends Exp
  final case class Diff(e1: Exp, e2: Exp) extends Exp
  final case class Eq(e1: Exp, e2: Exp) extends Exp
  final case class Minus(e: Exp) extends Exp
  final case class IsZero(e: Exp) extends Exp
  final case class Or(e1: Exp, e2: Exp) extends Exp
  final case class And(e1: Exp, e2: Exp) extends Exp
  final case class Not(e: Exp) extends Exp
  final case class IfThenElse(e1: Exp, e2: Exp, e3: Exp) extends Exp
  final case class Val(e: Exp) extends Exp

}
