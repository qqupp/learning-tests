package purecommands.semantic.evaluation.denotational

import purecommands.rts.{Env, Store}
import purecommands.semantic.domain.Conversions._
import purecommands.semantic.domain.Dval.{DLoc, Loc}
import purecommands.semantic.domain.Eval.{EBool, EInt}
import purecommands.semantic.domain.EvalOps._
import purecommands.semantic.domain.{Dval, Eval, Mval}
import purecommands.syntactic.domain.Exp
import purecommands.syntactic.domain.Exp._
import purecommands.syntactic.domain.Com
import purecommands.syntactic.domain.Com._

object Evaluation {

  type MvalStore = Store[Dval.Loc, Mval]

  def sem(e: Exp)(r: Env[Dval])(s: MvalStore): Eval = e match {
    case LitteralInt(a)    => EInt(a)
    case LitteralBool(a)   => EBool(a)
    case Den(i)     => dvalToEval(r.applyEnv(i))
    case IsZero(a)  => iszero(sem(a)(r)(s))
    case Eq(a, b)   => equ(sem(a)(r)(s), sem(b)(r)(s))
    case Prod(a, b) => mult(sem(a)(r)(s), sem(b)(r)(s))
    case Sum(a, b)  => plus(sem(a)(r)(s), sem(b)(r)(s))
    case Diff(a, b) => diff(sem(a)(r)(s), sem(b)(r)(s))
    case Minus(a)   => minus(sem(a)(r)(s))
    case And(a, b)  => et(sem(a)(r)(s), sem(b)(r)(s))
    case Or(a, b)   => vel(sem(a)(r)(s), sem(b)(r)(s))
    case Not(a)     => non(sem(a)(r)(s))
    case IfThenElse(a, b, c) => sem(a)(r)(s) match {
                                  case EBool(aa) => if (aa) sem (b) (r) (s) else sem (c) (r) (s)
                                  case _ => failWith(s"$a non a boolean value")
                                }
    case Val(a) => semDen(a)(r)(s) match {
                     case DLoc(l) => mvalToEval(s.applyStore(l))
                     case _ => failWith(s"$e not a variable")
                   }
  }

  def semDen(e: Exp)(r: Env[Dval])(s: MvalStore): Dval = e match {
    case Den(i) => r.applyEnv(i)
    case _ => evalToDval(sem(e)(r)(s))
  }

  def semCom(c: Com)(r: Env[Dval])(s: MvalStore): MvalStore = c match {
    case Assign(e1, e2) => semDen(e1)(r)(s) match {
                              case DLoc(l) => s.update(l, evalToMmval(sem(e2)(r)(s)))
                              case _ => failWith(s"$e1 wrong location in assignement")
                           }

    case Cifthenelse(e, c1, c2) => sem(e)(r)(s) match {
                                      case EBool(b) =>
                                        if (b)
                                          semComs(c1)(r)(s)
                                        else
                                          semComs(c2)(r)(s)
                                      case _ =>
                                        failWith(s"$e not a boolean value")
                                   }
    case While(e1, c1) => {
      def gamma(fi: MvalStore => MvalStore): MvalStore => MvalStore =
       (sigma: MvalStore) =>
         sem(e1)(r)(sigma) match {
           case EBool(b) =>
             if (b)
               fi(semComs(c1)(r)(sigma))
             else
               sigma
           case _ => failWith("not boolean while guard")
        }

      fix(gamma)(s)
    }
  }

  def fix[A, B](f: (A => B) => A => B): A => B  =
    x => f(fix(f))(x)

  def semComs(cc: List[Com])(r: Env[Dval])(s: MvalStore): MvalStore  = cc match {
    case Nil => s
    case c :: cs => semComs(cs)(r)( semCom(c)(r)(s) )
  }


}
