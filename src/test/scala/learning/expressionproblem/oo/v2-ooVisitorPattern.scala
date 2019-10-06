package learning.expressionproblem.oo

import StartingConditionOOVisitor._

object StartingConditionOOVisitor {

  abstract class Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T
  }

  class Banana(val p: Int) extends Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T = visitor.visitBanana(this)
  }

  class RectangularBoxSize2(val e1: Expression, val e2: Expression) extends Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T = visitor.visitRectanguraBoxSize2(this)
  }

  trait ExpressionVisitor[T] {
    def visitBanana(b: Banana): T
    def visitRectanguraBoxSize2(box: RectangularBoxSize2): T
  }


  class priceVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = b.p

    def visitRectanguraBoxSize2(box: RectangularBoxSize2): Int = box.e1.accept(this) + box.e2.accept(this) - 1
  }

}


object ExtensionOOVisitor {


  // Add new operation is easy with the visitor pattern but still now is difficult to add types

  class weightVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = 100

    def visitRectanguraBoxSize2(box: RectangularBoxSize2): Int = 2000
  }


}


object ExampleVisitor extends App {
  import ExtensionOOVisitor._

  val exp1 = new RectangularBoxSize2(
    new Banana(10),
    new RectangularBoxSize2(
      new Banana(10),
      new Banana(10)
    )
  )

  val w = new weightVisitor()
  println(exp1.accept(w))
}
