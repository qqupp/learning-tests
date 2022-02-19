package purecommands.rts


trait ModStack[T] {
  def emptystack(maxSize: Int, elem: T): ModStack[T]
  def push(t: T): Unit
  def pop: Unit
  def top: T
  def empty: Boolean
  def lungh: Int
  def svuota: Unit
  def access(elem: Int): T
  def setta(elem: Int, t: T): Unit
}

object ModStack {
  class Emptystack extends Exception
  class Fullstack extends Exception
  class Wrongaccess extends Exception
}
