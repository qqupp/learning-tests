package learning.typedmap

import learning.typedmap.TypedMap.Key

import scala.reflect.runtime.universe._

abstract class TypedMap[K](private val untyped: Map[Key[K, _], Any]) {

  def updated[V](k: Key[K, V], v: V): TypedMap[K] = new TypedMap(untyped.updated(k, v)) {}

  def get[V](k: Key[K, V]): Option[V] = untyped.get(k).asInstanceOf[Option[V]]

}

object TypedMap {

  def empty[K]: TypedMap[K] = new TypedMap[K](Map.empty) {}

  sealed abstract case class Key[K, V](keyValue: K, valueTypeTag: TypeTag[V])
  object Key extends KeySyntax {
    def apply[V](implicit ev: TypeTag[V]): Key[Unit, V]           = new Key[Unit, V]((), ev) {}
    def of[K, V](keyValue: K)(implicit ev: TypeTag[V]): Key[K, V] = new Key[K, V](keyValue, ev) {}

    def apply[K, V](keyValue: K, tt: WithType[V]): Key[K, V] = new Key[K, V](keyValue, tt.tag) {}
  }

  private[TypedMap] trait KeySyntax {
    sealed abstract case class WithType[T](tag: TypeTag[T])
    object WithType {
      def apply[T](implicit ev: TypeTag[T]): WithType[T] = new WithType[T](ev) {}
    }
  }

}
