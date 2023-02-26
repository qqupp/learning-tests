package simulatedHigherKinds

import scala.collection.mutable.ListBuffer


object Example extends App {

  val list: ListKind[Int] = new ListKind(ListBuffer(1, 2, 3))

  val optNone = new OptionKind[Int]
  val optSome = new OptionKind[Int]

  optSome.set(42)


  val int2String = (i: Int) => s"*** a string $i"

  val stringList: Higher[ListKind.Tpe.type, String] = Functors.map(ListKind.functorInstance)(list)(int2String)
  val stringOptNone: Higher[OptionKind.Tpe.type, String] = Functors.map(OptionKind.functorInstance)(optNone)(int2String)
  val stringOptSome: Higher[OptionKind.Tpe.type, String] = Functors.map(OptionKind.functorInstance)(optSome)(int2String)


  val listBufferResult: ListBuffer[String] = ListKind.downCast(stringList)
  listBufferResult.foreach(println(_))


  val opt1Result: Option[String] = OptionKind.downCast(stringOptNone)
  println(opt1Result)


  val opt2Result: Option[String] = OptionKind.downCast(stringOptSome)
  println(opt2Result)

}