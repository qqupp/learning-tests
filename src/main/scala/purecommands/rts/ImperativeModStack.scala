package purecommands.rts

final class ImperativeModStack[T] private(
                                           private var last: Int,
                                           private var undelying: Array[T]
                                         )  extends ModStack[T] {


  def emptystack(maxSize: Int, elem: T): ModStack[T] = new ImperativeModStack[T](-1, new Array(maxSize))

  def push(t: T): Unit =
    if (last >= undelying.length -1)
      throw new ModStack.Fullstack
    else {
      undelying(last + 1) = t
      last +=1
    }

  def pop: Unit =
    if (last <= -1)
      throw new ModStack.Emptystack
    else
      last -= 1

  def top: T =
     if (last <= -1)
      throw new ModStack.Emptystack
    else
      try {
        undelying(last)
      } catch {
        case _: Exception => throw new ModStack.Wrongaccess
      }

  def empty: Boolean =
    last == -1

  def lungh: Int = last

  def svuota: Unit = {last = -1}

  def access(elem: Int): T =
    if (elem > last || elem < 0)
      throw  new ModStack.Wrongaccess
   else
    undelying(elem)

  def setta(elem: Int, t: T): Unit =
    if (elem > last || elem < 0)
      throw  new ModStack.Wrongaccess
   else
    undelying(elem) = t
}

object ImperativeModStack {
  def emptystack[T](maxSize: Int, elem: T): ModStack[T] = new ImperativeModStack[T](-1, new Array(maxSize))
}