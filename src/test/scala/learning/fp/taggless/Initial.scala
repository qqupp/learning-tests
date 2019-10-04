package learning.fp.taggless

import learning.fp.taggless.model.User

// encodes operations as data
object InitialEncoding {

  import Algebra._

  //values
  object Algebra {
    sealed trait DBOps
    final case class SaveUser(user: User) extends DBOps
    final case class FindUserById(id: String) extends DBOps
    final case class Transaction(ops: Seq[DBOps]) extends DBOps
  }

  //composition of values
  object Expression {
    val user1 = User("1")
    val expr1: Algebra.DBOps = Transaction{
      List(
        SaveUser(user1),
        FindUserById(user1.id)
      )
    }
  }

  //function that takes values and produces a specific value
  object Interpreter {
    def evalToString(exp: DBOps): String = exp match {
      case SaveUser(u) => s"Save user $u"
      case FindUserById(id) => s"Find user $id"
      case Transaction(ops) => s"Transacting [${ops.map(evalToString)}]"
    }
  }

}


