package learning.minijson

object Show {
  import MiniJson._

  def show(j: Json): String = j match {
    case JNull => "null"
    case JTrue => "true"
    case JFalse => "false"
    case JInt(n) => s"$n"
    case JString(s) => s""""$s""""
    case JArray(items) =>
      items.map(show(_)).mkString("[ ", ", ", " ]")
    case JObject(elems) =>
      elems.map{ case (key, j) => s""""$key" : ${show(j)}"""}.mkString("{ ", ", ", " }")
  }

}