package learning.minijson

object zzzzDemoZipper extends App {

  import MiniJson._
  import Show._
  import Zipper._
  import zzzMoveUtils._

  val j1 = JObject(List(
    "str" -> JString("this is a string"),
    "int" -> JInt(0),
    "arr" -> JArray(List(JFalse, JTrue, JNull, JInt(10)))
  ))

  val c1 = moveTo(jsonToCursor(j1), D, R, R, D, R, R).right.get

  val j2 = cursorToJson(c1.addToArray(JString("added")).right.get)
  val j3 = cursorToJson(c1.set(JArray(List(JString("A1"), JString("A2")))))
  val j4 = cursorToJson(c1.delete.delete.delete.delete.delete.delete)

  println(show(j1))
  println(c1)

  println(show(j2))
  println(show(j3))
  println(show(j4))

  val backAndForth =
    cursorToJson(
      moveTo(jsonToCursor(j1), D, R, R, D, R, R, L, L, U, L, L, U).right.get
    )

  println(backAndForth == j1)
  println(show(backAndForth))


  /*
    new demo better usability
   */


  println("new interface more usable")
  println(show(j1))
  val j10 = j1.toCursor.goDownKey("arr").goDownAt(2).set(j1).toJson.right.get
  println(show(j10))

  val j11 =
    j1.toCursor
      .goDownKey("arr")
      .goDownAt(2)
      .set(j1)
      .goDownKey("int")
      .set(JString("abracadabra"))
      .goUp
      .goDownKey("arr")
      .goDownAt(0)
      .set(JString("newString"))
      .goUp
      .goLeft
      .goLeft
      .set(JString("changed"))
      .goRight
      .delete
      .toJson.right.get

  println(show(j11))

}
