package learning.expressionproblem.oo.visitor

object A {

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


  class PriceVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = 10

    def visitBox(box: Box): Int = box.e1.accept(this) + box.e2.accept(this) - 1
  }

}

object ATest extends App {
  import A._

  val priceVisitorInstance = new PriceVisitor

  val exp1 = new Box(new Banana, new Box( new Banana, new Banana))

  println(exp1.accept(priceVisitorInstance))
}


object C {
  import A._

  // Add new operation is easy with the visitor pattern but still now is difficult to add types

  class WeightVisitor extends ExpressionVisitor[Int] {
    def visitBanana(b: Banana): Int = 100

    def visitBox(box: Box): Int = box.e1.accept(this) + box.e2.accept(this) + 2000
  }

}


object CTest extends App {
  import A._
  import C._

  val exp1 = new Box(
    new Banana,
    new Box(
      new Banana,
      new Banana
    )
  )

  val w = new WeightVisitor()
  println(exp1.accept(w))
}
