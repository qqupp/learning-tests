package learning.tagless

import learning.tagless.TypedTaglessFinalInterpreter.TaglessFinal.VarCounter

object TypedTaglessFinalInterpreter extends App {

  // http://okmij.org/ftp/tagless-final/course/lecture.pdf

  // Interpreting typed higher-order languages
  object ProblemOfTags {

    trait Exp
    case class V(v: Var)           extends Exp // variables
    case class B(b: Boolean)       extends Exp // booleans
    case class L(e: Exp)           extends Exp // abstraction
    case class A(e1: Exp, e2: Exp) extends Exp // application

    trait Var
    case object VZ        extends Var
    case class VS(v: Var) extends Var

    val i1 = A(L(V(VZ)), B(true))

    // return type for the evaluator
    trait U
    case class UB(b: Boolean) extends U // this are the type tags refferred in the taglessfinal
    case class UA(a: U => U)  extends U

    // the presence of the type tags like UB and UA and run-time tag checking
    // are symptoms of the problem of embedding typed object languages

    def lookp[T](v: Var, env: List[T]): T =
      (v, env) match {
        case (VZ, x :: _)      => x
        case (VS(vv), _ :: xs) => lookp(vv, xs)
        case _                 => throw new Exception("inexhaustive pattern-match, unbound var")
      }

    def eval(env: List[U], e: Exp): U =
      e match {
        case V(v)      => lookp(v, env)
        case B(b)      => UB(b)
        case L(e)      => UA(x => eval(x :: env, e))
        case A(e1, e2) =>
          eval(env, e1) match {
            case UA(f) => f(eval(env, e2))
            case _     => throw new Exception("inexhaustive pattern-match, type error")
          }
      }

    val eval_i1 = eval(List(), i1)
  }

  object TaglessInitialEmbedding {

    // We move from Curry’s view of types to Church’sview:
    // ill-typed terms ‘do not exist’ and should not be representable.

    // Generic types for environment and Type of the embedded expression

    sealed trait Exp[Env, T]
    case class B[E](b: Boolean)                                 extends Exp[E, Boolean]
    case class V[E, T](v: Var[E, T])                            extends Exp[E, T]
    case class L[A, E, T](e: Exp[(A, E), T])                    extends Exp[E, A => T]
    case class A[AA, E, B](e1: Exp[E, AA => B], e2: Exp[E, AA]) extends Exp[E, B]

    // it is now impossible to create ill typed terms

    sealed trait Var[Env, T]
    case class VZ[E, T]()                extends Var[(T, E), T]
    case class VS[A, E, T](v: Var[E, T]) extends Var[(A, E), T]

    def lookp[H, T, E, O](v: Var[E, O], env: (H, T))(implicit ev: E =:= Tuple2[H, T]): O =
      (v, env) match {
        case (VZ(), (envHead, _))            => envHead.asInstanceOf[O]
        case (vs: VS[a, e, t], (_, envTail)) =>
          lookp(
            vs.v.asInstanceOf[Var[Tuple2[a, e], t]],
            envTail.asInstanceOf[Tuple2[a, e]]
          )
      }
//

    // lots of problems here in scala....  gadt ...
//    def eval[E, T](env: (T, E), e: Exp[E, T]): T =
//      e match {
//        case B(b)    => b
//        case V(v)    => lookp(v, env)
//        case L(e)    => x => eval((x, env), e)
//        case A(f, x) =>
//          eval(env, f)(eval(env, x))
//          //ff(eval(env, x))
//          ???
//      }

  }

  object TaglessFinal {
    //Recall that the final approach represents a term of an embedded language by
    // its value, or by a Haskell expression that computes that value.

    trait Symantics[F[_]] {
      def int(i: Int): F[Int]
      def add(i1: F[Int], i2: F[Int]): F[Int]
      def lam[A, B](e: F[A] => F[B]): F[A => B]
      def app[A, B](f: F[A => B], a: F[A]): F[B]
    }

    def th1[F[_]](S: Symantics[F]): F[Int] = {
      import S._
      add(int(1), int(2))
    }

    def th2[F[_]](S: Symantics[F]): F[Int => Int] = {
      import S._
      lam(x => add(x, x))
    }

    def th3[F[_]](S: Symantics[F]): F[(Int => Int) => Int] = {
      import S._
      lam(x => add(app(x, int(1)), int(2)))
    }

    def th4[F[_]](S: Symantics[F]) = {
      import S._
      app(th3(S), lam((x: F[Int]) => add(x, int(7))))
    }

    def th5[F[_]](S: Symantics[F]) = {
      import S._
      lam((x: F[Int]) => lam((y: F[Int]) => lam((z: F[Int]) => add(z, add(x, y)))))
    }

    // eval interpreter
    case class R[A](unR: A)
    val instanceR: Symantics[R] = new Symantics[R] {
      def int(i: Int): R[Int] = R(i)

      def add(i1: R[Int], i2: R[Int]): R[Int] = R(i1.unR + i2.unR)

      def lam[A, B](e: R[A] => R[B]): R[A => B] = R(a => e(R(a)).unR)

      def app[A, B](f: R[A => B], a: R[A]): R[B] = R(f.unR(a.unR))
    }

    // pretty printer interpreter
    case class VarCounter(x: Int) {
      def incr: VarCounter = VarCounter(x + 1)
    }
    case class S[A](unS: VarCounter => String)
    val instanceS: Symantics[S] = new Symantics[S] {
      def int(i: Int): S[Int] = S(_ => s"$i")

      def add(i1: S[Int], i2: S[Int]): S[Int] =
        S { vc =>
          s"( ${i1.unS(vc)} + ${i2.unS(vc)} )"
        }

      def lam[A, B](e: S[A] => S[B]): S[A => B] =
        S { vc =>
          val x = s"x${vc.x}"
          s"( $x => ${e(S(_ => x)).unS(vc.incr)} )"
        }

      def app[A, B](f: S[A => B], a: S[A]): S[B] =
        S { vc =>
          s"${f.unS(vc)}.apply( ${a.unS(vc)} )"
        }
    }

    // extending the language
    trait MulSYM[F[_]] {
      def mul(i1: F[Int], i2: F[Int]): F[Int]
    }

    trait BoolSYM[F[_]] {
      def bool(b: Boolean): F[Boolean]
      def equal(i1: F[Int], i2: F[Int]): F[Boolean]
      def ifThenElse[T](cond: F[Boolean], condTrue: => F[T], condFalse: => F[T]): F[T]
    }

    trait FixSYM[F[_]] {
      def fix[A, B](f: F[(A => B)] => F[A => B]): F[A => B]
    }

    // example composed symantics
    def tpow[F[_]](S: Symantics[F], M: MulSYM[F], B: BoolSYM[F], F: FixSYM[F]): F[Int => Int => Int] = {
      import S._
      import M._
      import B._
      import F._

      /*
       (x: Int) => fix((pow: Int => Int) => (exp: Int) =>  if (exp == 0) 1 else x * pow(exp - 1))
       */

      lam { (x: F[Int]) =>
        fix { (pow: F[Int => Int]) =>
          lam { (exp: F[Int]) =>
            ifThenElse(
              equal(exp, int(0)),
              int(1),
              mul(
                x,
                app(pow, add(exp, int(-1)))
              )
            )
          }
        }
      }
    }

    def tpow27[F[_]](S: Symantics[F], M: MulSYM[F], B: BoolSYM[F], F: FixSYM[F]): F[Int] = {
      import S._
      app(app(tpow(S, M, B, F), int(2)), int(7))
    }
    /*
    tpow27 with eval in S run with VarCount(0) will produce:
    ( x0 => fix self1 @ { ( x2 => if ( x2 == 0 ) then 1 else ( x0 * self1.apply( ( x2 + -1 ) ) ) ) } ).apply( 2 ).apply( 7 )

    tpow27 with eval in R will produce
    R(128)
     */

    // interpreters extensions
    val instanceRM: MulSYM[R] = new MulSYM[R] {
      def mul(i1: R[Int], i2: R[Int]): R[Int] = R(i1.unR * i2.unR)
    }

    val instanceRB: BoolSYM[R] = new BoolSYM[R] {
      def bool(b: Boolean): R[Boolean] = R(b)

      def equal(i1: R[Int], i2: R[Int]): R[Boolean] = R(i1.unR == i2.unR)

      def ifThenElse[T](cond: R[Boolean], condTrue: => R[T], condFalse: => R[T]): R[T] =
        R { if (cond.unR) condTrue.unR else condFalse.unR }
    }

    val instanceRF: FixSYM[R] = new FixSYM[R] {
      def fix[A, B](f: R[A => B] => R[A => B]): R[A => B] =
        R { (x: A) => f(fix(f)).unR(x) }
    }

    // implementation for S
    val instanceSM: MulSYM[S] = new MulSYM[S] {
      def mul(i1: S[Int], i2: S[Int]): S[Int] =
        S(vc => s"( ${i1.unS(vc)} * ${i2.unS(vc)} )")
    }

    val instanceSB: BoolSYM[S] = new BoolSYM[S] {
      def bool(b: Boolean): S[Boolean] = S(_ => s"$b")

      def equal(i1: S[Int], i2: S[Int]): S[Boolean] =
        S(vc => s"( ${i1.unS(vc)} == ${i2.unS(vc)} )")

      def ifThenElse[T](cond: S[Boolean], condTrue: => S[T], condFalse: => S[T]): S[T] =
        S(vc => s"if ${cond.unS(vc)} then ${condTrue.unS(vc)} else ${condFalse.unS(vc)}")
    }

    val instanceSF: FixSYM[S] = new FixSYM[S] {
      def fix[A, B](f: S[A => B] => S[A => B]): S[A => B] =
        S { vc =>
          val self = s"self${vc.x}"
          s"fix $self @ { ${f(S(_ => self)).unS(vc.incr)} }"
        }
    }
  }

  import TaglessFinal._
  val r = TaglessFinal.tpow27(instanceS, instanceSM, instanceSB, instanceSF)
  println(r.unS(VarCounter(0)))
}
