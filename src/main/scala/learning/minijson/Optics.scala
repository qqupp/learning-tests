package learning.minijson


object Optics {
  import MiniJson._

  trait LensOpt[S, B] {
    def get(prod: S): Option[B]
    def set(field: B)(prod: S): S
  }

  object LensOpt {
    def apply[A, B](proj: A => Option[B])(upd: B => A => A): LensOpt[A, B] = new LensOpt[A, B] {
      final def get(prod: A): Option[B] = proj(prod)

      final def set(field: B)(prod: A): A = upd(field)(prod)
    }
  }

  def composeLens[A, B, C](l1: LensOpt[A, B], l2: LensOpt[B, C]): LensOpt[A, C] = new LensOpt[A, C] {
    def get(prod: A): Option[C] = l1.get(prod) match {
      case None => None
      case Some(b) => l2.get(b)
    }

    def set(field: C)(prod: A): A = {
      l1.get(prod).fold(
        prod
      )(b =>
        l1.set(l2.set(field)(b))(prod)
      )
    }
  }

  implicit class LensOperations[A, B](l1: LensOpt[A, B]) {
    def and[C](l2: LensOpt[B, C]): LensOpt[A, C] = composeLens(l1, l2)
  }

  private def findAndSubst(k: String, j: Json, l: List[(String, Json)], acc: List[(String, Json)] = Nil): List[(String, Json)] = l match {
    case Nil => acc.reverse
    case (kk, jj) :: ls =>
      if (kk == k) {
        acc.reverse ++ ((k, j) :: ls)
      } else
        findAndSubst(k, j, ls, (kk, jj) :: acc)

  }

  private def substAt(idx: Int, j: Json, l: List[Json], acc: List[Json] = Nil): List[Json] = (idx, l) match {
    case (0, x::xs) => acc.reverse ++ (j :: xs)
    case (n, x::xs) => substAt(n -1, j, xs, x :: acc)
    case (_, Nil) => acc.reverse
  }

  val lensJInt = LensOpt[Json, Int]({ case JInt(i) => Some(i); case _ => None })(i => _ => JInt(i))
  val lensJString = LensOpt[Json, String]({ case JString(s) => Some(s); case _ => None })(s => _ => JString(s))
  val lensJArray = LensOpt[Json, List[Json]]({ case JArray(a) => Some(a); case _ => None })(itms => _ => JArray(itms))
  val lensJObject = LensOpt[Json, List[(String, Json)]]({ case JObject(e) => Some(e); case _ => None })(e => _ => JObject(e))
  def lensJObjectAt(key: String): LensOpt[Json, Json] =
    LensOpt[Json, Json]({
      case JObject(elems) => elems.collectFirst{ case (`key`, j) => j}
      case _ => None
    })(
      newJ => {
        case JObject(elems) => JObject(findAndSubst(key, newJ, elems))
        case j => j
      }
    )

  def lensJArrayAt(idx: Int): LensOpt[Json, Json] =
    LensOpt[Json, Json]({
      case JArray(items) => if (idx < items.length && idx >= 0) Some(items(idx)) else None
      case _ => None
    }
    )(
      newJ => {
        case JArray(items) =>
          JArray(substAt(idx, newJ, items))
        case j => j
      }
    )
}