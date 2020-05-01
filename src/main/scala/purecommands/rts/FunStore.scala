package purecommands.rts

final case class FunStore[T](private val lastLoc: Int, private val f: Int => T) extends Store[Int, T] {

  def emptyStore(t: T): Store[Int, T] = FunStore(0, _ => t)

  def allocate(t: T): (Int, Store[Int, T]) = {
    val updatedLoc = lastLoc + 1
    (updatedLoc, FunStore(updatedLoc, l => if (l == updatedLoc) t else f(l)))
  }

  def update(loc: Int, t: T): Store[Int, T] =
    FunStore(lastLoc, l => if(l == loc) t else f(l) )

  def applyStore(loc: Int): T = f(loc)

}

object FunStore {

  def emptyStore[T](t: T): Store[Int, T] = FunStore(0, _ => t)

}
