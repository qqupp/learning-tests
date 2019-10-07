package learning.expressionproblem.oo

import StartingConditionOOVisitor._

object StartingConditionOOVisitor {

  abstract class Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T
  }

  class Banana extends Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T = visitor.visitBanana(this)
  }

  class Box(val e1: Expression, val e2: Expression) extends Expression {
    def accept[T](visitor: ExpressionVisitor[T]): T = visitor.visitBox(this)
  }

  trait ExpressionVisitor[T] {
    def visitBanana(b: Banana): T
    def visitBox(box: Box): T
  }


  class priceVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = 10

    def visitBox(box: Box): Int = box.e1.accept(this) + box.e2.accept(this) - 1
  }

}


object ExtensionOOVisitor {


  // Add new operation is easy with the visitor pattern but still now is difficult to add types

  class weightVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = 100

    def visitBox(box: Box): Int = 2000
  }


}


object ExampleVisitor extends App {
  import ExtensionOOVisitor._

  val exp1 = new Box(
    new Banana,
    new Box(
      new Banana,
      new Banana
    )
  )

  val w = new weightVisitor()
  println(exp1.accept(w))
}
