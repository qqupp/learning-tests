package learning

object TupleList extends App {

  object homogeneousVersion {
    sealed trait HVar
    case class HVZ()        extends HVar
    case class HVS(v: HVar) extends HVar

    sealed trait U
    case class US(s: String) extends U
    case class UC(c: Char)   extends U
    case class UI(i: Int)    extends U

    type Env = List[U]

    def lookup(v: HVar, env: Env): U =
      (v, env) match {
        case (HVZ(), e :: _)   => e
        case (HVS(s), _ :: es) => lookup(s, es)
        case _                 => throw new Exception("empty list")
      }

  }

  //from Typed Tagless Final Interpreters
  // 3  Interpreting Typed Higher-Order Languages

  sealed trait Var[Env, T]
  case class VZ[E, T]()                extends Var[(T, E), T]
  case class VS[A, E, T](v: Var[E, T]) extends Var[(A, E), T]

  implicit class EnvOps[T](val t: T) extends AnyVal {
    def :::[H](h: H): Tuple2[H, T] = (h, t)
  }

  def lookp[H, T, E, O](v: Var[E, O], env: (H, T))(implicit ev: E =:= Tuple2[H, T]): O =
    (v, env) match {
      case (VZ(), (envHead, _))            => envHead.asInstanceOf[O]
      case (vs: VS[a, e, t], (_, envTail)) =>
        lookp(
          vs.v.asInstanceOf[Var[Tuple2[a, e], t]],
          envTail.asInstanceOf[Tuple2[a, e]]
        )
    }

  val myEnv: (String, (Int, (Char, Unit)))                = "one" ::: 2 ::: '3' ::: ()
  val myVar01: Var[(String, (Int, (Char, Unit))), String] = VZ()
  val myVar02: Var[(String, (Int, (Char, Unit))), Int]    = VS(VZ())
  val myVar03: Var[(String, (Int, (Char, Unit))), Char]   = VS(VS(VZ()))

  val result01: String = lookp(myVar01, myEnv)
  val result02: Int    = lookp(myVar02, myEnv)
  val result03: Char   = lookp(myVar03, myEnv)

  println(result01)
  println(result02)
  println(result03)

}
