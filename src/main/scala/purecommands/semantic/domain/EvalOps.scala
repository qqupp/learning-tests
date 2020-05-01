package purecommands.semantic.domain

import purecommands.semantic.domain.Eval._

object EvalOps {

  def failWith(s: String) = throw new Exception(s)

  def partial[T](pf: PartialFunction[Eval, T]): Eval => T =
    pf orElse {
      case x => failWith(s"$x not a valid type")
    }

  val minus: Eval => EInt =
    partial {
      case EInt(v) => EInt(-v)
    }

  val iszero: Eval => EBool =
    partial {
      case EInt(v) => EBool(v == 0)
    }

  def equ(x: Eval, y: Eval): EBool =
    partial {
      case EInt(v1) => partial {
        case EInt(v2) =>
          EBool(v1 == v2)
      }(y)
    }(x)

  def plus(x: Eval, y: Eval): EInt =
    partial {
      case EInt(v1) => partial {
        case EInt(v2) =>
          EInt(v1 + v2)
      }(y)
    }(x)

  def diff(x: Eval, y: Eval): EInt =
    partial {
      case EInt(v1) => partial {
        case EInt(v2) =>
          EInt(v1 - v2)
      }(y)
    }(x)

  def mult(x: Eval, y: Eval): EInt =
    partial {
      case EInt(v1) => partial {
        case EInt(v2) =>
          EInt(v1 * v2)
      }(y)
    }(x)

  def et(x: Eval, y: Eval): EBool =
    partial {
      case EBool(v1) => partial {
        case EBool(v2) =>
          EBool(v1 && v2)
      }(y)
    }(x)


  def vel(x: Eval, y: Eval): EBool =
    partial {
      case EBool(v1) => partial {
        case EBool(v2) =>
          EBool(v1 || v2)
      }(y)
    }(x)

  def non: Eval => EBool =
    partial {
        case EBool(v1) => EBool(!v1)
    }

}
