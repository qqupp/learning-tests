package purecommands.semantic.domain

import purecommands.semantic.domain.Eval._
import purecommands.semantic.domain.Mval._
import purecommands.semantic.domain.Dval._

object Conversions {

  case class NonStorable() extends Exception
  case class NonExpressible() extends Exception

  def evalToMmval(e: Eval): Mval = e match {
    case EInt(n) => MInt(n)
    case EBool(n) => MBool(n)
    case _ => throw NonStorable()
  }

  def mvalToEval(m: Mval): Eval = m match {
    case MInt(n)  => EInt(n)
    case MBool(n)  => EBool(n)
    case _  => Novalue
  }

  def evalToDval(e: Eval): Dval = e match {
    case EInt(v) => DInt(v)
    case EBool(v) => DBool(v)
    case Novalue => Unbound
  }

  def dvalToEval(d: Dval): Eval = d match {
    case DInt(v) => EInt(v)
    case DBool(v) => EBool(v)
    case Unbound => Novalue
    case _ => throw NonExpressible()
  }

}
