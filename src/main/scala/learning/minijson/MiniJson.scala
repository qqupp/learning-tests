package learning.minijson

object MiniJson {

  sealed trait Json

  final case object JNull extends Json

  final case object JTrue extends Json
  final case object JFalse extends Json

  final case class JInt(i: Int) extends Json

  final case class JString(s: String) extends Json

  final case class JArray(items: List[Json]) extends Json

  final case class JObject(elems: List[(String, Json)]) extends Json

}





