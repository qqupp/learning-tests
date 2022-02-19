package purecommands.semantic.evaluation.iterative

import purecommands.rts._
import purecommands.semantic.domain.Conversions._
import purecommands.semantic.domain.Dval.{DLoc, Unbound}
import purecommands.semantic.domain.Eval.{EBool, EInt, Novalue}
import purecommands.semantic.domain.EvalOps._
import purecommands.semantic.domain.Mval.Undefined
import purecommands.semantic.domain.{Dval, Eval, Mval}
import purecommands.syntactic.domain.Com._
import purecommands.syntactic.domain.Exp._
import purecommands.syntactic.domain.{Com, Exp}

object Evaluation {


  trait labeledconstruct

  case class Expr1(value: Exp) extends labeledconstruct
  case class Expr2(value: Exp) extends labeledconstruct
  case class Exprd1(value: Exp) extends labeledconstruct
  case class Exprd2(value: Exp) extends labeledconstruct
  case class Com1(value: Com) extends labeledconstruct
  case class Com2(value: Com) extends labeledconstruct
  case class Coml(value: List[labeledconstruct]) extends labeledconstruct

  val continuation: ModStack[labeledconstruct] =
    ImperativeModStack.emptystack(
      StackSizes.cframesize,
      Expr1(LitteralInt(0))
    )

  val tempstack: ModStack[Eval] =
   ImperativeModStack.emptystack(
     StackSizes.tframesize,
     Novalue
   )

  val tempdstack: ModStack[Dval] =
    ImperativeModStack.emptystack(
      StackSizes.tdframesize,
      Unbound
    )

  var globalstore: Store[Int, Mval.Undefined.type] = FunStore.emptyStore(Undefined)

  def labelcom(dl: List[Com]): Coml = {
      var dlr = dl
      var ldlr = List[Com1]()
      while (dlr.nonEmpty) {
        val i = dlr.head
        ldlr = ldlr ++ List(Com1(i))
        dlr = dlr.tail
      }
    Coml(ldlr)
  }


  def itsemcl(rho: Env[Dval]) =  ???


  def pushToStack[A, B](stack: ModStack[A], f: B => A, bs: B *) =
    bs.foreach(b => stack.push(f(b)))

  def pushToStack[B](stack: ModStack[B], bs: B *) =
    bs.foreach(b => stack.push(b))

  def applyFunTo[A](stack: ModStack[A], f: A => A) = {
    val arg1 = stack.top
    stack.pop
    stack.push(f(arg1))
  }

  def applyFun2To[A](stack: ModStack[A], f: (A, A) => A) = {
    val arg1 = stack.top
    stack.pop
    val arg2 = stack.top
    stack.pop
    stack.push(f(arg1, arg2))
  }


  def itsem(rho: Env[Dval]): Unit =
    continuation.top match {
      case Expr1(x) => {
          continuation.pop
          continuation.push(Expr2(x))
          x match {
            case IsZero(a)  => pushToStack(continuation, Expr1, a)
            case Eq(a, b)   => pushToStack(continuation, Expr1, a, b)
            case Prod(a, b) => pushToStack(continuation, Expr1, a, b)
            case Sum(a, b)  => pushToStack(continuation, Expr1, a, b)
            case Diff(a, b) => pushToStack(continuation, Expr1, a, b)
            case Minus(a)   => pushToStack(continuation, Expr1, a)
            case And(a, b)  => pushToStack(continuation, Expr1, a, b)
            case Or(a, b)   => pushToStack(continuation, Expr1, a, b)
            case Not(a)     => pushToStack(continuation, Expr1, a)
            case IfThenElse(a, b, c) => pushToStack(continuation, Expr1, a)
            case Val(a) =>  pushToStack(continuation, Exprd1, a)
            case _ => ()
          }
      }

      case Expr2(x) => {
          continuation.pop
          x match {
            case LitteralInt(a)    => pushToStack(tempstack, EInt(a))
            case LitteralBool(a)   => pushToStack(tempstack, EBool(a))
            case Den(i)            => pushToStack(tempstack, dvalToEval(rho.applyEnv(i)))
            case IsZero(a)  => applyFunTo(tempstack, iszero)
            case Eq(a, b)   => applyFun2To(tempstack, equ)
            case Prod(a, b) => applyFun2To(tempstack, mult)
            case Sum(a, b)  => applyFun2To(tempstack, plus)
            case Diff(a, b) => applyFun2To(tempstack, diff)
            case Minus(a)   => applyFunTo(tempstack, minus)
            case And(a, b)  => applyFun2To(tempstack, et)
            case Or(a, b)   => applyFun2To(tempstack, vel)
            case Not(a)     => applyFunTo(tempstack, non)
            case IfThenElse(a, b, c) => {
                                          val arg1 = tempstack.top
                                          tempstack.pop
                                          arg1 match {
                                            case EBool(aa) =>
                                                              if (aa)
                                                                pushToStack(continuation, Expr1, b)
                                                              else
                                                                pushToStack(continuation, Expr1, c)
                                            case _ => failWith(s"$a non a boolean value")
                                          }
                                        }
            case Val(a) => {
                             val arg1: Dval = tempdstack.top
                             tempstack.pop
                             arg1 match {
                               case DLoc(l) => pushToStack(tempstack, mvalToEval(globalstore.applyStore(l)))
                               case _ => failWith(s"$a not a variable")
                             }
                           }
            case _ => failWith(s"$x is not a valid case for Expr2")
          }

      }
    }

  def itsemden(rho: Env[Dval]) = ???



  type MvalStore = Store[Dval.Loc, Mval]



  def sem(e: Exp, r: Env[Dval], s: MvalStore): Eval = e match {
    case LitteralInt(a)    => EInt(a)
    case LitteralBool(a)   => EBool(a)
    case Den(i)     => dvalToEval(r.applyEnv(i))
    case IsZero(a)  => iszero(sem(a, r, s))
    case Eq(a, b)   => equ(sem(a, r, s), sem(b, r, s))
    case Prod(a, b) => mult(sem(a, r, s), sem(b, r, s))
    case Sum(a, b)  => plus(sem(a, r, s), sem(b, r, s))
    case Diff(a, b) => diff(sem(a, r, s), sem(b, r, s))
    case Minus(a)   => minus(sem(a, r, s))
    case And(a, b)  => et(sem(a, r, s), sem(b, r, s))
    case Or(a, b)   => vel(sem(a, r, s), sem(b, r, s))
    case Not(a)     => non(sem(a, r, s))
    case IfThenElse(a, b, c) => sem(a, r, s) match {
                                  case EBool(aa) => if (aa) sem (b, r, s) else sem (c, r, s)
                                  case _ => failWith(s"$a non a boolean value")
                                }
    case Val(a) => semDen(a, r, s) match {
                     case DLoc(l) => mvalToEval(s.applyStore(l))
                     case _ => failWith(s"$e not a variable")
                   }
  }

  def semDen(e: Exp, r: Env[Dval], s: MvalStore): Dval = e match {
    case Den(i) => r.applyEnv(i)
    case _ => evalToDval(sem(e, r, s))
  }

  def semCom(c: Com, r: Env[Dval], s: MvalStore): MvalStore = c match {
    case Assign(e1, e2) => semDen(e1, r, s) match {
                              case DLoc(l) => s.update(l, evalToMmval(sem(e2, r, s)))
                              case _ => failWith(s"$e1 wrong location in assignement")
                           }

    case Cifthenelse(e, c1, c2) => sem(e, r, s) match {
                                      case EBool(b) =>
                                        if (b)
                                          semComs(c1, r, s)
                                        else
                                          semComs(c2, r, s)
                                      case _ =>
                                        failWith(s"$e not a boolean value")
                                   }
    case While(e1, c1) =>
         sem(e1, r, s) match {
           case EBool(b) =>
             if (b)
               semComs(c1 ++ List(While(e1, c1)), r, s)
             else
               s
           case _ => failWith("not boolean while guard")
        }
  }

  def semComs(cc: List[Com], r: Env[Dval], s: MvalStore): MvalStore  = cc match {
    case Nil => s
    case c :: cs => semComs(cs, r,  semCom(c, r, s))
  }

}
