package learning.minijson

object zzzzDemoParser extends App {
  import MiniJson._
  import Parser._
  import Show._

  val json = Parser.fromString(genBigJson(200, "9"))

  def genBigJson(x: Int, acc: String): String  =
    if (x <= 0)
      acc
    else
      genBigJson(x - 1, s"[ 1, $acc, 2 ]")

  println(show(json.right.get))

}
