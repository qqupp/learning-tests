package learning.minijson

object Zipper {
  import MiniJson._

  def jsonToCursor(j: Json): Cursor = Cursor(j, Top)
  def cursorToJson(c: Cursor): Json = c match {
    case Cursor(j, Top) => j
    case _ =>   cursorToJson(c.goUp.right.get)
  }

  type Error = String
  type Result[T] = Either[Error, T]

  sealed trait Path
  final case object Top extends Path
  final case class ArrayNode(left: List[Json], up: Path, right: List[Json]) extends Path
  final case class ObjectNode(left: List[(String, Json)], currentKey: String, up: Path, right: List[(String, Json)]) extends Path


  final case class Cursor(current: Json, path: Path) { self =>

    def toJson: Json = cursorToJson(self)

    def set(j: Json): Cursor = Cursor(j, path)

    def addToArray(j: Json): Result[Cursor] = path match {
      case ArrayNode(l, up, r) => Right(
        Cursor(j, ArrayNode(l, up, current::r))
      )
      case Top => current match {
        case JArray(items) => Right(
          Cursor(j, ArrayNode(Nil, Top, items))
        )
        case _ => Left("No")
      }
      case _ => Left("No")
    }

    def addToObj(key: String, j: Json): Result[Cursor] = path match {
      case ObjectNode(l, ck, up, r) => Right(
        Cursor(j, ObjectNode(l, key, up, ck -> current :: r))
      )
      case Top => current match {
        case JObject(elems) => Right(
          Cursor(j, ObjectNode(Nil, key, Top, elems))
        )
        case _ => Left("No")
      }
      case _ => Left("No")
    }

    def delete: Cursor = path match {
      case Top => Cursor(JNull, Top)
      case ArrayNode(l::ls, up, r) => Cursor(l, ArrayNode(ls, up, r))
      case ArrayNode(Nil, up, r::rs) => Cursor(r, ArrayNode(Nil, up, rs))
      case ArrayNode(Nil, up, Nil) => Cursor(JArray(List()), up)
      case ObjectNode( (k,l) :: ls, _, up, r) => Cursor(l, ObjectNode(ls, k, up, r))
      case ObjectNode(Nil, _, up, (k,r) :: rs) => Cursor(r, ObjectNode(Nil, k, up, rs))
      case ObjectNode(Nil, _, up, Nil) => Cursor(JObject(Nil), up)
    }

    def goDown: Result[Cursor] = current match {
      case JArray(Nil) => Left("down on empty array")
      case JArray(fst :: rest) =>
        Right(
          Cursor(fst, ArrayNode(Nil, path, rest))
        )
      case JObject(Nil) => Left("down on empty object")
      case JObject((key, json) :: rest) =>
        Right(
          Cursor(json, ObjectNode(Nil, key, path, rest))
        )
      case _ => Left("down on non recursive structure")
    }

    private def splitObjWithKey(key: String, list: List[(String, Json)], acc: List[(String, Json)] = Nil): (List[(String, Json)], Option[(String, Json)], List[(String, Json)]) =
      list match {
        case Nil => (acc, None, Nil)
        case (k, j) :: ll =>
          if (k == key)
            (acc, Some((k, j)), ll)
          else
            splitObjWithKey(key, ll, (k, j) :: acc)
      }

    def goDownKey(key: String): Result[Cursor] = current match {
      case JObject(elems) =>
        splitObjWithKey(key, elems) match {
          case (l, Some((k, j)), r) => Right(Cursor(j,ObjectNode(l, k, path, r)))
          case _ => Left("no key found")
        }
      case _ => Left("Not a JObject")
    }

    private def splitArrayAt(idx: Int, items: List[Json], left: List[Json] = Nil): (List[Json], Option[Json], List[Json]) = (idx, items) match {
      case (0, j::js) => (left, Some(j), js)
      case (n, j::js) => splitArrayAt(n - 1, js, j:: left)
      case (_, Nil)  => (left, None, Nil)
    }

    def goDownAt(idx: Int): Result[Cursor] = current match {
      case JArray(items) =>
        splitArrayAt(idx, items) match {
          case (l, Some(j), r) => Right(Cursor(j, ArrayNode(l, path, r)))
          case _ => Left("invalid index")
        }
      case _ => Left("Not a JArray")
    }

    def goLeft: Result[Cursor] = path match {
      case ArrayNode(Nil, _, _) => Left("empty array")
      case ArrayNode(j :: js, up, right) =>
        Right(Cursor(j, ArrayNode(js, up, current :: right)))
      case ObjectNode(Nil, _, _, _) => Left("empty object")
      case ObjectNode((k, j) :: js, cKey, up, right) =>
        Right(Cursor(j, ObjectNode(js, k, up, (cKey, current) :: right)))
      case _ => Left("no elements left")
    }

    def goRight: Result[Cursor] = path match {
      case ArrayNode(_, _, Nil) => Left("empty array")
      case ArrayNode(left, up, j :: js) =>
        Right(Cursor(j, ArrayNode(current :: left, up, js)))
      case ObjectNode(_, _, _, Nil) => Left("empty object")
      case ObjectNode(left, cKey, up, (k, j) :: js) =>
        Right(Cursor(j, ObjectNode((cKey, current) :: left, k, up, js)))
      case _ => Left("no elements left")
    }

    def goUp: Result[Cursor] = path match {
      case ArrayNode(left, up, right) =>
        Right(Cursor(JArray(left.reverse ++ (current :: right)), up))
      case ObjectNode(left, cKey, up, right) =>
        Right(Cursor(JObject(left.reverse ++ (cKey -> current :: right)), up))
      case _ => Left("no up")
    }

  }

  implicit class JsonZipperOps(val j: Json) extends AnyVal {
    def toCursor: Cursor = jsonToCursor(j)
  }

  implicit class ZipperResultOps(val rc: Result[Cursor]) extends AnyVal{
    def goUp: Result[Cursor] = rc.flatMap(_.goUp)
    def goDown: Result[Cursor] = rc.flatMap(_.goDown)
    def goLeft: Result[Cursor] = rc.flatMap(_.goLeft)
    def goRight: Result[Cursor] = rc.flatMap(_.goRight)
    def goDownKey(key: String): Result[Cursor] = rc.flatMap(_.goDownKey(key))
    def goDownAt(idx: Int): Result[Cursor] = rc.flatMap(_.goDownAt(idx))
    def set(j: Json): Result[Cursor] = rc.map(_.set(j))
    def addToArray(j: Json): Result[Cursor] = rc.flatMap(_.addToArray(j))
    def addToOjb(key: String, j: Json): Result[Cursor] = rc.flatMap(_.addToObj(key, j))
    def delete: Result[Cursor] = rc.map(_.delete)
    def toJson: Result[Json] = rc.map(_.toJson)
  }

}

