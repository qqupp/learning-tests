package learning.somet

case class Username private(val value: String) extends AnyVal
object Username {
  def mkUsername(value: String): Option[Username] =
    if (!value.isEmpty) Some(Username(value))
    else None
}