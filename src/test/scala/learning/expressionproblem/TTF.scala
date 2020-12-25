package learning.expressionproblem

object TTF {

  object taggedInitialEncoding {
    trait Expr
    case class IntConst(i: Int)        extends Expr
    case class Lambda(e: Expr => Expr) extends Expr
    case class Apply(f: Expr, x: Expr) extends Expr
    case class Add(e1: Expr, e2: Expr) extends Expr

    // an expression is a value of tagged types
    val e = Apply(Lambda(x => Add(IntConst(1), x)), IntConst(2))

    // this allows to define malformed terms eg Apply(IntConst, IntConst)
    // so evaluation is partial

    trait EvalResult
    case class IntResult(i: Int)          extends EvalResult
    case class FunResult(f: Expr => Expr) extends EvalResult

    def eval: Expr => Option[EvalResult] = {
      case IntConst(i) => Some(IntResult(i))
      case Apply(f, x) =>
        eval(f).flatMap {
          case FunResult(fok) => eval(fok(x))
          case _              => None // type error at runtime
        }
      // similarly other cases
    }

  }

  object taglessInitailEncoding {
    // GADT to express only valid expressions
    trait Expr[T]
    case class IntConst(i: Int)                                     extends Expr[Int]
    case class Lambda[A, B](e: Expr[A] => Expr[B])                  extends Expr[Expr[A] => Expr[B]]
    case class Apply[A, B](f: Expr[Expr[A] => Expr[B]], x: Expr[A]) extends Expr[B]
    case class Add(e1: Expr[Int], e2: Expr[Int])                    extends Expr[Int]

    def eval[T]: Expr[T] => T = {
      case IntConst(i) => i
      case Lambda(f)   => f
      case Apply(f, x) => eval(eval(f)(x))
      case Add(e1, e2) => eval(e1) + eval(e2)
    }

    val e = Apply(Lambda((x: Expr[Int]) => Add(IntConst(1), x)), IntConst(2))

    // impossible to express Apply(IntConst, IntConst) because it would not typececk in
    // the original language
  }

  object taglessFinalEncoding {
    // type classes for the algebra
    trait Expr[Repr[_]] {
      def intConst(i: Int): Repr[Int]
      def lambda[A, B](e: Repr[A] => Repr[B]): Repr[A => B]
      def apply[A, B](f: Repr[A => B], x: Repr[A]): Repr[B]
      def add(e1: Repr[Int], e2: Repr[Int]): Repr[Int]
    }

    // interpreters
    type Id[A] = A
    val evalIntepreter: Expr[Id] = new Expr[Id] {
      def intConst(i: Int): Id[Int]                   = i
      def lambda[A, B](e: Id[A] => Id[B]): Id[A => B] = e
      def apply[A, B](f: Id[A => B], x: Id[A]): Id[B] = f(x)
      def add(e1: Id[Int], e2: Id[Int]): Id[Int]      = e1 + e2
    }

    def e[Repr[_]](interpreter: Expr[Repr]): Repr[Int] = {
      import interpreter._
      apply(lambda((x: Repr[Int]) => add(intConst(1), x)), intConst(2))
    }

    // now it's easy to extend with new algebras and/or new interpreters

    // extend the algebra
    trait ExtendedExpr[Repr[_]] {
      def mul(e1: Repr[Int], e2: Repr[Int]): Repr[Int]
    }

    val evalExtendedIntepreter: ExtendedExpr[Id] = new ExtendedExpr[Id] {
      def mul(e1: Id[Int], e2: Id[Int]): Id[Int] = e1 * e2
    }

    def e2[Repr[_]](i: Expr[Repr], ii: ExtendedExpr[Repr]) = {
      import i._
      import ii._
      mul(e(i), intConst(4))
    }

  }

}
