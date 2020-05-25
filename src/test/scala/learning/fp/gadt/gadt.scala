package learning.fp.gadt

object GADT extends App {

  sealed trait Expr[T]
  case class I(i: Int) extends Expr[Int]
  case class B(b: Boolean) extends Expr[Boolean]
  case class Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
  case class Mul(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
  case class Eq(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean]

  def eval[T](expr: Expr[T]): T = expr match {
    case I(i) => i
    case B(b) => b
    case Add(e1, e2) => eval(e1) + eval(e2)
    case Mul(e1, e2) => eval(e1) * eval(e2)
    case Eq(e1, e2) => eval(e1) == eval(e2)
  }

  val value = Eq(Mul(I(2),I(3)),Add(I(5),I(1)))

  val result = eval(value)
  println(result)

}

object ADT {

  sealed trait NExpr
  case class I(i: Int)
  case class Add(e1: NExpr, e2: NExpr) extends NExpr
  case class Mul(e1: NExpr, e2: NExpr) extends NExpr

  sealed trait BExpr
  case class B(b: Boolean) extends BExpr
  case class Eq(e1: NExpr, e2: NExpr) extends BExpr

  sealed trait Expr
  case class BoolExpr(e: BExpr) extends Expr
  case class NatExpr(e: NExpr) extends Expr
//
//  val evalN : NExpr => Int  = {
//    case I( n) => n
//    case Add(e1, e2) => evalN(e1) + evalN(e2)
//    case Mul(e1, e2) => evalN(e1) * evalN(e2)
//  }
//
//  val evalB: BExpr => Boolean = {
//    case B(b) => b
//    case Eq(e1, e2) => evalN(e1) == evalN(e2)
//  }
//
//  sealed trait Evaled
//  case class EvaledB(b: Boolean) extends Evaled
//  case class EvaledN(i: Int) extends Evaled
//
//  val eval: Expr => Evaled = {
//    case BoolExpr(b) => EvaledB(evalB(b))
//    case NatExpr(n) => EvaledN(evalN(n))
//  }

}