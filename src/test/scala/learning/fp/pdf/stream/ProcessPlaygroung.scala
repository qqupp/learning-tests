package learning.fp.pdf.stream


object ProcessPlaygroung extends App {

  import Process._

  val p0: Process[Int, Int] = lift(identity)
  val p1: Process[Int, String] = lift[Int, String](i => i.toString)
  val p2: Process[String, String] = lift[String, String](s => s"processing $s")
  def printerP[T]: Process[T, T] = lift[T, T](t => {println(t); t})

  val p01: Process[Int, Int] = p0.pipe(filter(i => i % 2 == 0))
  val p4 = p01.pipe(p1.pipe(p2.pipe(printerP)))

  val result = p4.apply(Stream(1,2,3,4, 5, 6, 7)).toList


  //val t: Process[Any, Int] = unit(1).repeat pipe printerP

  //t(Stream.from(1)).take(10)
}
