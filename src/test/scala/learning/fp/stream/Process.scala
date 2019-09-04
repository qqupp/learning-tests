package learning.fp.stream

import scala.annotation.tailrec

trait Process[I, O] {

  def apply(input: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Emit(h, t) => h.toStream append t(input)
    case Await(recv, finalizer) => input match {
      case h #:: t => recv(h)(t)
      case _ => finalizer(input)
    }
  }

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    emitAll(Seq(head), tail)

  def unit[O](o: => O): Process[I,O] = emit(o)


  def pipe[O2](p2: Process[O, O2]):Process[I, O2] = (this, p2) match {
    case (Halt(), _) => Halt()
    case (_, Halt()) => Halt()
    case (Await(recv, next), p2) => Await(i => recv(i).pipe(p2), next.pipe(p2))
    case (p1, Emit(h2, t2)) => Emit[I, O2](h2, p1.pipe(t2))
    case (Emit(h, next1), Await(recv, next2)) =>
      val yyy: Process[O, O2] = h.map(recv).foldRight(next2)((fst, snd) => fst.append(snd))
      val rightHead: Seq[O2] = Seq()
      val rightTail: Process[I, O2] = next1.pipe(yyy)
      Emit[I, O2](rightHead, rightTail)
    case _ => ???
  }


  final def emitAll[A, B](h1: Seq[B], t1: Process[A, B]): Emit[A, B] = t1 match {
    case Emit(h2, t2) => Emit(h1 ++ h2, t2)
    case _ => Emit(h1, t1)
  }

  def map[O2](f: O => O2): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(head, tail) => Emit(head.map(f), tail.map(f))
    case Await(recv, finalizer) => Await( i => recv(i).map(f), finalizer.map(f))
  }

  def append(next: Process[I, O]): Process[I, O] = this match {
    case Halt() => next
    case Emit(h, t) => emitAll(h, t.append(next))
    case Await(recv, finalizer) => Await(recv andThen( _.append(next)), finalizer.append(next))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(head, tail) => {
      val r: Seq[Process[I, O2]] = head.map(f)
      val t: Process[I, O2] = tail.flatMap(f)
      r.foldRight(t)( (p1, p2) => p1.append(p2) )
    }
    case Await(recv, finalizer) => {
      val newRcv: I => Process[I, O2] = i => recv(i).flatMap(f)
      Await(newRcv, finalizer.flatMap(f))
    }
  }
}

object Process {

  def lift[I,O](f: I => O): Process[I,O] =
    Await((i: I) => Emit(Seq(f(i)), lift(f)))

}

case class Emit[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: I => Process[I, O], finalizer: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]



object T extends App {

  val p1: Process[Int, String] = Process.lift[Int, String](i => i.toString)
  val p2: Process[String, String] = Process.lift[String, String](s => s"processing $s")
  val p3: Process[String, Unit] = Process.lift[String, Unit](s => println(s))

  val p4: Process[Int, Unit] = p1.pipe(p2).pipe(p3)

  p4.apply(Stream(1,2,3,4))push push push con il push saLAVATT
}