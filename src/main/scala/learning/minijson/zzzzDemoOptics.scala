package learning.minijson

object zzzzDemoOptics extends App {

  import MiniJson._
  import Show._
  import Optics._

  val j1 = JObject(List(
    "str" -> JString("this is a string"),
    "int" -> JInt(0),
    "arr" -> JArray(List(JFalse, JTrue, JNull, JInt(10)))
  ))

  val mylens: LensOpt[Json, Json] =  lensJObjectAt("arr") and lensJArrayAt(2)
  val j2 = mylens.set(j1)(j1)
  val j3 = (mylens and mylens).set(JString("abracadabra"))(j2)

  println(show(j1))
  println(show(j2))
  println(show(j3))

  val lens2 =  mylens and lensJObjectAt("int") and lensJInt
  val j4 = lens2.set(42)(j3)

  println(show(j4))

}
