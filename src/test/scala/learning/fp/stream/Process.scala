package learning.fp.stream

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait Process[I, O] {

  def apply(input: Stream[I]): Stream[O] = Process.run( this, input)

  def pipe[O2](p2: Process[O, O2]):Process[I, O2] = Process.pipe(this, p2)

  def repeat: Process[I,O] = Process.repeat(this)

  def append(next: Process[I, O]): Process[I, O] = Process.concat(this, next)

  def map[O2](f: O => O2): Process[I, O2] = Process.map(this)(f)

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = Process.flatMap(this)(f)
}

case class Emit[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: I => Process[I, O], finalizer: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {

  def run[I, O](p: Process[I, O], input: Stream[I]): Stream[O] = p match {
    case Halt() => Stream()
    case Emit(head, tail) => head.toStream append run(tail, input)
    case Await(recv, finalizer) => input match {
      case h #:: t => run(recv(h), t)
      case _ => run(finalizer, input)
    }
  }

  def lift[I,O](f: I => O): Process[I,O] =
    Await((i: I) => Emit(Seq(f(i)), lift(f)))

  final def pipe[A, B, C](p1: Process[A, B], p2: Process[B, C]): Process[A, C] = (p1, p2) match {
    case (Halt(), Await(recvBpBC, pBC))                   => Halt()
    case (Halt(), Emit(seqC, pBC))                        => Emit(seqC, p1.pipe(pBC))
    case (Halt(), Halt())                                 => Halt()

    case (Await(recvApAB, nextpAB), Halt())               => Halt()
    case (Await(recvApAB, nextpAB), Emit(seqC, pBC))      => Emit(seqC, p1.pipe(pBC))
    case (Await(recvApAB, nextpAB), Await(recvBpBC, pBC)) => Await(a => (recvApAB(a) append(nextpAB)).pipe(p2), nextpAB.pipe(p2))

    case (Emit(seqB, pAB), Emit(seqC, pBC))               => Emit(seqC, Emit(seqB).append(pAB).pipe(pBC))
    case (Emit(seqB, pAB), Halt())                        => Halt()
    case (Emit(bs, pAB),   Await(recvBpBC, pBC))          =>
      val folded: Process[B, C] = bs.map(recvBpBC).foldRight(pBC)((l, r) => l.append(r))
      pAB.pipe(folded)
  }

  def concat[I, O](fst: Process[I, O], snd: Process[I, O]): Process[I, O] = fst match {
    case Halt() => snd
    case Emit(h, t) => Emit(h, concat(t, snd))
    case Await(recv, finalizer) => Await(recv andThen(concat(_, snd)), concat(finalizer, snd))
  }

  def map[I, O1, O2](p: Process[I, O1])(f: O1 => O2): Process[I, O2] = p match {
    case Halt() => Halt()
    case Emit(head, tail) => Emit(head.map(f), tail.map(f))
    case Await(recv, finalizer) => Await(i => recv(i).map(f), finalizer.map(f))
  }

  def flatMap[I, O1, O2](p: Process[I, O1])(f: O1 => Process[I, O2]): Process[I, O2] = p match {
    case Halt() => Halt()
    case Emit(head, tail) =>
      val r: Seq[Process[I, O2]] = head.map(f)
      val t: Process[I, O2] = tail.flatMap(f)
      r.foldRight(t)( (p1, p2) => p1.append(p2) )

    case Await(recv, finalizer) =>
      val newRcv: I => Process[I, O2] = i => recv(i).flatMap(f)
      Await(newRcv, finalizer.flatMap(f))
    }

  @tailrec
  final def emitAll[A, B](h1: Seq[B], t1: Process[A, B]): Emit[A, B] = t1 match {
    case Emit(h2, t2) => emitAll(h1 ++ h2, t2)
    case _ => Emit(h1, t1)
  }

  def emit[I, O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    emitAll(Seq(head), tail)

  def unit[O](o: => O): Process[Any,O] = emit[Any, O](o)


  def repeat[I, O](p: Process[I, O]): Process[I,O] = {

    def go(pg: Process[I, O]): Process[I, O] = pg match {
      case Halt() => go(p)
      case Await(recv,fb) => Await(recv andThen go, fb)
      case Emit(h, t) => Emit(h, go(t))
    }

    go(p)
  }

  def filter[I](f: I => Boolean): Process[I,I] = {
    Await[I,I](i => if (f(i)) Process.emit(i) else Halt()) repeat
  }


}


object T extends App {

  val p0: Process[Int, Int] = Process.lift(identity)
  val p1: Process[Int, String] = Process.lift[Int, String](i => i.toString)
  val p2: Process[String, String] = Process.lift[String, String](s => s"processing $s")
  val p3: Process[String, Unit] = Process.lift[String, Unit](s => println(s))

  val p01: Process[Int, Int] = p0.pipe(Process.filter(i => i % 2 == 0))
  val p4 = p01.pipe(p1.pipe(p2.pipe(p3)))

  val result = p4.apply(Stream(1,2,3,4, 5, 6, 7)).toList

  println(result)
}