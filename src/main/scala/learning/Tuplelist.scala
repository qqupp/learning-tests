package learning

object TupleList extends App {

  //from Typed Tagless Final Interpreters
  // 3  Interpreting Typed Higher-Order Languages

  sealed trait Var[Env, T]
  case class VZ[E, T]()                extends Var[Env[T, E], T]
  case class VS[A, E, T](v: Var[E, T]) extends Var[Env[A, E], T]

  sealed trait Env[H, T]
  case class EnvCons[H, T <: Env[_, _]](h: H, t: T) extends Env[H, T]
  case class EnvNil()                               extends Env[Unit, Nothing]

  implicit class EnvOps[T <: Env[_, _]](val t: T) extends AnyVal {
    def :::[H](h: H): Env[H, T] = EnvCons(h, t)
  }

  def lookp[H, T, E, O](v: Var[E, O], env: Env[H, T])(implicit ev: E =:= Env[H, T]): O =
    (v, env) match {
      case (VZ(), EnvCons(h, _))            => h.asInstanceOf[O]
      case (vv: VS[a, e, t], EnvCons(_, t)) => lookp(vv.v.asInstanceOf[Var[Env[a, e], t]], t.asInstanceOf[Env[a, e]])
      case _                                => ???
    }

  val myEnv: Env[String, Env[Int, Env[Char, EnvNil]]]                = "one" ::: 2 ::: '3' ::: EnvNil()
  val myVar01: Var[Env[String, Env[Int, Env[Char, EnvNil]]], String] = VZ()
  val myVar02: Var[Env[String, Env[Int, Env[Char, EnvNil]]], Int]    = VS(VZ())
  val myVar03: Var[Env[String, Env[Int, Env[Char, EnvNil]]], Char]   = VS(VS(VZ()))

  val result01: String = lookp(myVar01, myEnv)
  val result02: Int    = lookp(myVar02, myEnv)
  val result03: Char   = lookp(myVar03, myEnv)

  println(result01)
  println(result02)
  println(result03)

}
