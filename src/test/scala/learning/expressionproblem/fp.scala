package learning.expressionproblem

class fp {

  object initialEncoding {
    trait Expression
    case object Banana                            extends Expression
    case class Box(e: Expression, e2: Expression) extends Expression

    val e01 = Box(Banana, Banana)

    def priceInterpreter(e: Expression): Int  =
      e match {
        case Banana => ???
        // etc
      }
    def weightInterpreter(e: Expression): Int = ???
  }

  // final encoding
  trait Expression[Repr] {
    def banana: Repr
    def box(e1: Repr, e2: Repr): Repr
  }

  def expression01[Repr](interpreter: Expression[Repr]): Repr = {
    import interpreter._
    box(banana, banana)
  }

  case class Price(value: Int)
  val priceExpressionInterpreter: Expression[Price] = new Expression[Price] {
    def banana: Price = Price(10)

    def box(e1: Price, e2: Price): Price = Price(e1.value + e2.value)
  }

  // add new operation
  case class Weight(value: Int)
  val weightExpressionInterpreter: Expression[Weight] = new Expression[Weight] {
    def banana: Weight = Weight(2)

    def box(e1: Weight, e2: Weight): Weight = Weight(20)
  }

  // the same expression can be interpreted in different ways
  val price: Price   = expression01(priceExpressionInterpreter)
  val weight: Weight = expression01(weightExpressionInterpreter)

  // add new data
  trait ExtendedExpression[Repr] {
    def apple: Repr
  }

  val priceExtendedExpressionInterpreter: ExtendedExpression[Price] = new ExtendedExpression[Price] {
    def apple: Price = Price(12)
  }

  val weightExtendedExpressionInterpreter: ExtendedExpression[Weight] = new ExtendedExpression[Weight] {
    def apple: Weight = Weight(1)
  }

  // expression02 contains the original expression without the need to recompile anything
  def expression02[Repr](
      interpreter: Expression[Repr],
      interpreterExtended: ExtendedExpression[Repr]
  ): Repr = {
    import interpreter._
    import interpreterExtended._
    box(apple, box(expression01(interpreter), apple))
  }

  val price2: Price   = expression02(priceExpressionInterpreter, priceExtendedExpressionInterpreter)
  val weight2: Weight = expression02(weightExpressionInterpreter, weightExtendedExpressionInterpreter)

  // making interpreters implicit whould reduce the verbosity of the code.

}
