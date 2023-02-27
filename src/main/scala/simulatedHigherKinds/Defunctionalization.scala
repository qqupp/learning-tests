package simulatedHigherKinds

import simulatedHigherKinds.Defunctionalization.HKT.TypeList

object Defunctionalization {

  /*
  *  Defunctionalization at Work
  *  ISSN 0909-0878
  */

  /*
    higher-order programs can be defunctionalized into first- order programs,
    as proposed by Reynolds in the early 1970’s [43]. In a defunctionalized program,
    first-class functions are represented with first- order data types:
      a first-class function is introduced with a constructor holding the values
      of the free variables of a function abstraction, and it is eliminated
      with a case expression dispatching over the corresponding constructors.
   */

  object `A sample higher-order program with a static number of closures` {
    def aux(f: Int => Int): Int = f(1) + f(10)

    def main(x: Int, y: Int, b: Boolean): Int =
      aux(z => x + z) * aux(z => if (b) y + z else y - z)


    //defunctionalized
    sealed trait Lambdas

    case class Lam1(x: Int) extends Lambdas // free variable in z => x + z

    case class Lam2(b: Boolean, y: Int) extends Lambdas // free variables in z => if (b) y + z else y - z

    def lambdasApply(l: Lambdas, z: Int): Int =
      l match {
        case Lam1(x) => z + x
        case Lam2(b, y) => if (b) y + z else y - z
      }

    def auxFirstOrder(l: Lambdas): Int =
      lambdasApply(l, 1) + lambdasApply(l, 10)

    def mainFirstOrder(x: Int, y: Int, b: Boolean): Int =
      auxFirstOrder(Lam1(x)) * auxFirstOrder(Lam2(b, y))

  }


  object `dynamic number of closures` {
    def aux(i: Int, f: (Int => Int)): Int =
      f(i)


    def main(i: Int, js: List[Int]): List[Int] = {

      def walk(l: List[Int]): List[Int] = l match {
        case Nil => Nil
        case j :: js => aux(i, x => x + j) :: walk(js)
      }

      walk(js)
    }


    /*
      defunctionalization
       - 1 data type with only one constructor since there is only one function abstraction
     */

    sealed trait Lambdas
    case class Lam1(j: Int)   extends Lambdas//free variable in x => x + j

    def lambdasApply(l: Lambdas, x: Int): Int = l match {
      case Lam1(j) => x + j
    }

    def auxFirstOrder(i: Int, f: Lambdas): Int =
      lambdasApply(f, i)

    def mainFirstOrder(i: Int, js: List[Int]): List[Int] = {

      def walk(l: List[Int]): List[Int] = l match {
        case Nil => Nil
        case j :: js => auxFirstOrder(i, Lam1(j)) :: walk(js)
      }

      walk(js)
    }
  }

  /*
  In a higher-order program, first-class functions arise as instances of function abstractions.
  All these function abstractions can be enumerated in a whole program.
  Defunctionalization is thus a whole-program transformation where function types are replaced
  by an enumeration of the function abstractions in this program.
   */



  object `Flattening a binary tree into a list` {

    sealed trait BinaryTree[T]
    case class Leaf[T](t: T) extends BinaryTree[T]
    case class Node[T](left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

    def cons[T](t: T)(ts: List[T]): List[T] =
      t :: ts

    def flatten[T](t: BinaryTree[T]): List[T] = {

      def walk(t: BinaryTree[T]): List[T] => List[T] = t match {
        case Leaf(x) =>
          cons(x)(_)
        case Node(l, r) =>
          walk(l) compose walk(r)

      }

      walk(t)(Nil)
    }

//
//    /* eta expnading */
//    def flatten_ee[T](t: BinaryTree[T]): List[T] = {
//
//      def walk(t: BinaryTree[T], a: List[T]): List[T] = t match {
//        case Leaf(x) => x :: a
//        case Node(l, r) => walk(l, walk(r, a))
//      }
//
//      walk(t, Nil)
//    }

    /* defunctionalised flatten */
    sealed trait Lambdas[T]
    case class Lam1[T](t: T) extends Lambdas[T]
    case class Lam2[T](l: Lambdas[T], r: Lambdas[T]) extends Lambdas[T]

    def lambdasApply[T](lam: Lambdas[T], xs: List[T]): List[T] =
      lam match {
        case Lam1(t) => t :: xs
        case Lam2(f1, f2) => lambdasApply(f1, lambdasApply(f2, xs))
      }

    def consDef[T](x: T): Lambdas[T] = Lam1(x)
    def composeDef[T](f1: Lambdas[T], f2: Lambdas[T]): Lambdas[T] = Lam2(f1, f2)

    def flattendFirstOrder[T](t: BinaryTree[T]): List[T] = {

      def walk(t: BinaryTree[T]): Lambdas[T] = t match {
        case Leaf(x) => consDef(x)
        case Node(l, r) => composeDef(walk(l), walk(r))
      }

      lambdasApply(walk(t), Nil)
    }

  }



  object CPS {
    /*
      recognise 0n 1n for n belonging to N
     */
    def rec0(xs: List[Int]): Boolean = {

      object Not extends Exception

      def walk(list: List[Int]): List[Int] = list match {
        case (0 :: xs1) =>
          walk(xs1) match {
            case 1 :: xs2 => xs2
            case _ => throw Not
          }
        case xs => xs
      }

      try {
        walk(xs) == Nil
      } catch {
        case Not => false
      }

    }
  }


  /* based on defunctionalization */
  object HKT {


    type HK[F[_], X] = F[X]

    trait Functor[F[_]] {
      def map[A, B](fa: HK[F, A], f: A => B): HK[F, B]
    }

    val listInstance: Functor[List] = new Functor[List] {
      override def map[A, B](fa: HK[List, A], f: A => B): HK[List, B] = fa match {
        case Nil => Nil
        case a :: as => f(a) :: map(as, f)
      }
    }

    /* defunctionalization for types */

    sealed trait TTag
    case object TypeList extends TTag
    case object TypeOption extends TTag

    class Higher[F <: TTag, X] private(val tpe: F, val content: Any)

    object  Higher {
      def downCastList[X](l: Higher[TypeList.type, X]): List[X] =
        l.content.asInstanceOf[List[X]]

      def upCastList[X](l: List[X]): Higher[TypeList.type, X] =
        new Higher[TypeList.type, X](TypeList, l)

      def downCastOption[X](l: Higher[TypeOption.type, X]): Option[X] =
        l.content.asInstanceOf[Option[X]]

      def upCastOption[X](o: Option[X]): Higher[TypeOption.type, X] =
        new Higher[TypeOption.type, X](TypeOption, o)
    }

    trait FunctorFirstOrder[F <: TTag] {
      def map[A, B](fa: Higher[F, A], f: A => B): Higher[F, B]
    }

    import Higher._

    val lamListInstance = new FunctorFirstOrder[TypeList.type] {
      override def map[A, B](fa: Higher[TypeList.type , A], f: A => B): Higher[TypeList.type, B] =
        downCastList(fa) match {
          case Nil => upCastList(Nil)
          case a :: as => upCastList(f(a) :: downCastList(map(upCastList(as), f)))
        }
    }

    val lamOptionInstance = new FunctorFirstOrder[TypeOption.type] {
      override def map[A, B](fa: Higher[TypeOption.type, A], f: A => B): Higher[TypeOption.type, B] =
        downCastOption(fa) match {
          case None => upCastOption(None)
          case Some(x) => upCastOption(Some(f(x)))
        }
    }






  }

}
