package learning.fp.online.stream

sealed trait Process[I, O] {

  def apply(input: Stream[I]): Stream[O] = Process.run(this, input)

  def pipe[O2](p2: Process[O, O2]): Process[I, O2] = Process.pipe(this, p2)

  final def repeat: Process[I, O] = {

    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await(
          {
            case None => recv(None)
            case i    => go(recv(i))
          }
        )
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def append(next: Process[I, O]): Process[I, O] = Process.concat(this, next)

  def map[O2](f: O => O2): Process[I, O2] = Process.map(this)(f)

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
    Process.flatMap(this)(f)
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {

  def run[I, O](p: Process[I, O], input: Stream[I]): Stream[O] = p match {
    case Halt()           => Stream()
    case Emit(head, tail) => head #:: run(tail, input)
    case Await(recv) =>
      input match {
        case h #:: t => run(recv(Some(h)), t)
        case _       => run(recv(None), input)
      }
  }

  def lift[I, O](f: I => O): Process[I, O] =
    Await((oi: Option[I]) =>
      oi match {
        case Some(i) => Emit(f(i), lift(f))
        case None    => Halt()
    })

  final def pipe[A, B, C](p1: Process[A, B], p2: Process[B, C]): Process[A, C] =
    (p1, p2) match {
      case (Halt(), Await(recvBpBC)) => Halt()
      case (Halt(), Emit(c, pBC))    => Emit(c, p1.pipe(pBC))
      case (Halt(), Halt())          => Halt()

      case (Await(recvApAB), Halt())       => Halt()
      case (Await(recvApAB), Emit(c, pBC)) => Emit(c, p1.pipe(pBC))
      case (Await(recvApAB), Await(recvBpBC)) =>
        Await(opa => recvApAB(opa).pipe(p2))

      case (Emit(b, pAB), Emit(c, pBC)) =>
        Emit(c, Emit(b).append(pAB).pipe(pBC))
      case (Emit(b, pAB), Halt())          => Halt()
      case (Emit(b, pAB), Await(recvBpBC)) => pAB.pipe(recvBpBC(Some(b)))

    }

  def concat[I, O](fst: Process[I, O], snd: Process[I, O]): Process[I, O] =
    fst match {
      case Halt()      => snd
      case Emit(h, t)  => Emit(h, concat(t, snd))
      case Await(recv) => Await(recv andThen (concat(_, snd)))
    }

  def map[I, O1, O2](p: Process[I, O1])(f: O1 => O2): Process[I, O2] = p match {
    case Halt()           => Halt()
    case Emit(head, tail) => Emit(f(head), tail.map(f))
    case Await(recv)      => Await(i => recv(i).map(f))
  }

  def flatMap[I, O1, O2](p: Process[I, O1])(
      f: O1 => Process[I, O2]): Process[I, O2] =
    p match {
      case Halt()           => Halt()
      case Await(recv)      => Await(i => recv(i).flatMap(f))
      case Emit(head, tail) => concat(f(head), tail.flatMap(f))
    }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def unit[O](o: => O): Process[Any, O] = emit[Any, O](o)

  def filter[I](f: I => Boolean): Process[I, I] = {
    Await[I, I]({
      case Some(i) if f(i) => Process.emit(i)
      case _               => Halt()
    }) repeat
  }

  def take[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
      if (n <= 0)
        Halt()
      else
        Await({
          case Some(i) => Emit(i, go(n - 1))
          case None    => Halt()
        })

    go(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
      if (n <= 0)
        lift(identity)
      else
        Await({
          case None    => Halt()
          case Some(i) => go(n - 1)
        })

    go(n)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    Await({
      case None => Halt()
      case Some(i) =>
        if (f(i))
          Emit(i, takeWhile(f))
        else
          Halt()
    })
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    Await({
      case None => Halt()
      case Some(i) =>
        if (f(i))
          dropWhile(f)
        else
          Emit(i, lift(identity))
    })
  }

  def count[I]: Process[I,Int] = {
    def go(n: Int): Process[I,Int] =
      Await({
        case Some(i) => Emit(n + 1, go(n + 1))
        case None => Halt()
      })

    go(0)
  }
}

object OnlinePlayground extends App {

  import Process._
  val p0: Process[Int, Int] = lift(identity)
  val p1: Process[Int, String] = lift[Int, String](i => i.toString)
  val p2: Process[String, String] = lift[String, String](s => s"processing $s")
  def printerP[T]: Process[T, T] = lift[T, T](t => { println(t); t })

  val p01: Process[Int, Int] = p0.pipe(filter(i => i % 2 == 0))
  val p4 = p01.pipe(p1.pipe(p2.pipe(printerP)))

  val result = p4.apply(Stream(1, 2, 3, 4, 5, 6, 7)).toList

  def t4[T] = take[T](4)

  val result2 = dropWhile[Int](_ <= 14).pipe(printerP[Int].pipe(takeWhile(_ <= 25))).pipe(count)(Stream.from(1)).toList

  println(result2)
}
