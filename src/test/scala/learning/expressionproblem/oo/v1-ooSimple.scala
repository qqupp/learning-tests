package learning.expressionproblem.oo
import StartingExtensionOOSimple._

object StartingExtensionOOSimple {

  abstract class Expression {
    def price: Int
  }

  class Banana extends Expression {
    def price: Int = 10
  }

  class Box(e1: Expression, e2: Expression) extends Expression {
    def price: Int = e1.price + e2.price -1
  }

}


object ExtensionOOSimple {


  // Add new types is easy in oo
  class Apple(p: Int) extends Expression {
    def price: Int = p
  }


  // how do you add new operations without recompiling?

}

object ExtensionOOSimpleTest extends App {
  import learning.expressionproblem.oo.ExtensionOOSimple._

  val exp1 = new Box(
                          new Banana(),
                          new Box(
                            new Banana(),
                            new Apple(17)
                          )
              )

  println(exp1.price)
}
