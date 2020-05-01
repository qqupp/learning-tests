package purecommands.rts

final case class FunEnv[T](private val r: String => T) extends Env[T] {

  def emptyEnv(t: T): Env[T] = FunEnv(_ => t)

  def bind(name: String, value: T): Env[T] =
    FunEnv(n => if (n == name) value else r(n))

  def bindList(nameValues: List[(String, T)]): Env[T] =
      nameValues.foldLeft(this : Env[T]){ case (accEnv, (name, value)) =>
        accEnv.bind(name,value)
      }

  def applyEnv(name: String): T = r(name)
}

object FunEnv {
  def emptyEnv[T](t: T): Env[T] = FunEnv[T](_ => t)
}
