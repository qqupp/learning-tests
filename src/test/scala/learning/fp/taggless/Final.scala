package learning.fp.taggless

import learning.fp.taggless.model.User

//encodes operations as functions
object FinalEncoding {

  // interface with verbs a.k.a. functions
  object Algebra {

    trait DBOps[T] {
      def saveUser(user: User): T
      def findUserById(id: String): T
      def transaction(ops: Seq[T]): T
    }

  }

  // function that takes an instance of the algebra
  object Expression {
    val user1 = User("1")

    def expr1[T](algebraInstance: Algebra.DBOps[T]): T =
      algebraInstance.transaction{
        List(
          algebraInstance.saveUser(user1),
          algebraInstance.findUserById(user1.id)
        )
      }
  }

  // instance of the algebra
  object Interpreter {
    val evalToString: Algebra.DBOps[String] = new Algebra.DBOps[String] {
      def saveUser(user: User) = s"Save user $user"
      def findUserById(id: String) = s"Find user $id"
      def transaction(ops: Seq[String]) = s"Transacting [${ops}]"
    }
  }

}
