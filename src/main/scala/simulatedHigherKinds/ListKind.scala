package simulatedHigherKinds

import scala.collection.mutable.ListBuffer

class ListKind[T](lb: ListBuffer[T]) extends Higher[ListKind.Tpe.type, T] with scala.collection.mutable.Seq[T] {
  val underline: ListBuffer[T] = lb

  override def update(idx: Int, elem: T): Unit = underline.update(idx, elem)

  override def apply(i: Int): T = underline.apply(i)

  override def length: Int = underline.length

  override def iterator: Iterator[T] = underline.iterator
}


object ListKind {
  object Tpe

  def downCast[T](hkt: Higher[ListKind.Tpe.type, T]): ListBuffer[T] = {
    val lk: ListKind[T] = hkt.asInstanceOf[ListKind[T]]
    lk.underline
  }


  def functorInstance[A]: FunctorTypeClass[Tpe.type] = new FunctorTypeClass[ListKind.Tpe.type] {

    override def map[A, B](fa: Higher[Tpe.type, A])(f: A => B): Higher[Tpe.type, B] = {
      val lb = new ListBuffer[B]()
      val la: ListBuffer[A] = downCast[A](fa)
      for (a <- la) {
        lb.append((f(a)))
      }
      new ListKind[B](lb)
    }
  }

}




