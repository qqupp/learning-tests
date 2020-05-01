package purecommands.syntactic.domain

sealed trait Com

object Com {

  final case class Assign(e1: Exp, e2: Exp) extends Com
  final case class Cifthenelse(e1: Exp, c1: List[Com], c2: List[Com]) extends Com
  final case class While(e1: Exp, c1: List[Com]) extends Com

}
