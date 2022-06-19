package dependeincygraph

import cats.free.Free
import cats.{Monad, effect, ~>}
import scala.reflect.runtime.universe._


object DependencyGraph {

  /*
    Nodes contain Effects
    FlatMap is the natural connection between nodes
   */
  trait DependencyGraphOps[F[_], A]
  case class Node[F[_], A](name: String, value: F[A], tag: TypeTag[A]) extends DependencyGraphOps[F, A]

  type DependencyGraphMonad[F[_], A] =
    Free[({type DG[X] = DependencyGraphOps[F, X]})#DG, A]


  def create[F[_], A: TypeTag](name: String, value: F[A]): DependencyGraphMonad[F, A] =
    Free.liftF(Node(name, value, implicitly[TypeTag[A]]))


  def wiring[F[_]] : ({type DG[X] = DependencyGraphOps[F, X]})#DG ~> F =
    new (({type DG[X] = DependencyGraphOps[F, X]})#DG ~> F) {
      override def apply[A](fa: DependencyGraphOps[F, A]): F[A] = fa match {
        case Node(_, v, _) => v.asInstanceOf[F[A]]
      }
    }

  def subst[F[_]](substitutions: List[Node[F, _]]):
    ({type DG[X] = DependencyGraphOps[F, X]})#DG ~> F =
    new (({type DG[X] = DependencyGraphOps[F, X]})#DG ~> F) {
      override def apply[A](fa: DependencyGraphOps[F, A]): F[A] = fa match {
        case Node(name, v, tt) =>
          substitutions.find(s => s.name == name && s.tag.equals(tt))
            .fold(
              v.asInstanceOf[F[A]]
            )(
              node => node.value.asInstanceOf[F[A]]
            )
      }
    }


  def wire[F[_]: Monad, A](g: DependencyGraphMonad[F, A]): F[A] =
    g.foldMap(wiring[F])


  def wireSub[F[_]: Monad, A](g: DependencyGraphMonad[F, A], substitutions: List[Node[F, _]]): F[A] =
    g.foldMap(subst(substitutions))
}


object TestFM extends App {
  import DependencyGraph._

  val io1 = effect.IO{println("do first thing"); 10}
  val io2 = effect.IO{println("do second thing"); false}
  val io3 = effect.IO{println("do third thing"); "result44"}

  val myGraphExample: DependencyGraphMonad[effect.IO, String]  =
    for {
      n1 <- create("label1", io1)
      n2 <- create("label2", io2)
      n3 <- create("label3", io3)
    } yield s"$n1 $n2 $n3"


  val io2Substitution = effect.IO{println("do something completely different"); true}
  val subN2 = Node("label2", io2Substitution, implicitly[TypeTag[Boolean]])


  import cats.implicits._
  val r = wire(myGraphExample *> myGraphExample)
  val rSub = wireSub(myGraphExample *> myGraphExample, List(subN2))


  r.map(println(_)).unsafeRunSync()

  ///
  println("**********")
  println("do substitutions and run")
  println("**********")

  rSub.map(println(_)).unsafeRunSync()
}