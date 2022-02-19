package learning.typedmap

import learning.typedmap.Env._

import scala.reflect.runtime.universe._

abstract case class Env[+T] private( private val env: Map[TypeTag[Any], Has[Any]]) {
  def add[A, B >: A](tpe: Tpe[A], value: B)(implicit tag: TypeTag[A]): Env[T with Has[A]] = {
    new Env[T with Has[A]](env.updated(tag.asInstanceOf[TypeTag[Any]], Has(value))) {}
  }

  def get[A](tpe: Tpe[A])(implicit tag: TypeTag[A], ev: T <:< Has[A]): Has[A] = {
    env.getOrElse(
      tag.asInstanceOf[TypeTag[Any]],
      throw
        new RuntimeException(
          s"""Bug ???
           |Asking for:
           |  tpe: $tpe,
           |  tag: $tag
           |but internal map cointains following:"
           |${env.map{ case (tt, v) => s"  $tt -> $v"}.mkString("\n")}.
           |""".stripMargin
        )
     ).asInstanceOf[Has[A]]
  }
}

object Env {
  import scala.reflect.runtime.universe._

  def apply[A, B >: A](tpe: Tpe[A], value: B)(implicit tag: TypeTag[A]): Env[Has[A]] =
    (new Env(Map()) {}).add(tpe, value).asInstanceOf[Env[Has[A]]]

  sealed trait Tpe[T]
  object Tpe {
    def apply[T] = new Tpe[T] {}
  }

  case class Has[T](value: T)

  private[typedmap] case class Key[T](tag: TypeTag[T])

}
