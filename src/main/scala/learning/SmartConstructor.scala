package learning

object SmartConstructor extends App {

  val invalidEmail = Email.fromString("notAvalidEmail")
  val validEmail = Email.fromString("someguy@email.com")


  println(invalidEmail)
  println(validEmail)


  // cant copy a valid one
  
}



sealed abstract case class Email(value: String)

object Email {
  def fromString(str: String): Option[Email] = {
    if (str.contains("@"))
      Some(new Email(str) {})
    else
      None
  }
}