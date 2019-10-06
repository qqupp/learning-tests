package learning.expressionproblem.oo
import StartingConditionOONaive._

object StartingConditionOONaive {

  abstract class Expression {
    def price: Int
  }

  class Banana(p: Int) extends Expression {
    def price: Int = p
  }

  class RectangularBoxSize2(e1: Expression, e2: Expression) extends Expression {
    def price: Int = e1.price + e2.price -1
  }

}


object ExtensionOONaive {


  // Add new types is easy in oo
  class Apple(p: Int) extends Expression {
    def price: Int = p
  }


  // how do you add new operations without recompiling?

}

object ExampleNaive extends App {
  import learning.expressionproblem.oo.ExtensionOONaive._

  val exp1 = new RectangularBoxSize2(
                          new Banana(10),
                          new RectangularBoxSize2(
                            new Banana(10),
                            new Apple(10)
                          )
              )

  println(exp1.price)
}
