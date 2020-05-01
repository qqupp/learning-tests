package purecommands.rts

trait Env[T] {
  def emptyEnv(t: T): Env[T]
  def bind(name: String, value: T): Env[T]
  def bindList(nameValues: List[(String, T)]): Env[T]
  def applyEnv(name: String): T
}
