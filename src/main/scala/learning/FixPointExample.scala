package learning

object FixPointExample {

  def recursiveFactorial(x: Int): Int = if (x == 0) 1 else x * recursiveFactorial(x -1)

  val gammaFactorial: (Int => Int) => Int => Int =
    (f: Int => Int) => (x: Int) =>  if (x == 0) 1 else x * f(x -1)

  val bottomOfIntToInt: Int => Int = _ => throw new Exception("Bottom of Int => Int")
  val factorial0: Int => Int = gammaFactorial(bottomOfIntToInt)
  val factorial1: Int => Int = gammaFactorial(factorial0)
  val factorial2: Int => Int = gammaFactorial(factorial1)
  val factorial3: Int => Int = gammaFactorial(factorial2)
  val factorial4: Int => Int = gammaFactorial(factorial3)
  val factorial5: Int => Int = gammaFactorial(factorial4)
  val factorial6: Int => Int = gammaFactorial(factorial5)
  val factorial7: Int => Int = gammaFactorial(factorial6)

  /*
      ...
   */

  def fix[A, B](f: (A => B) => A => B): A => B  =
    x => f(fix(f))(x)

  val factorial = fix(gammaFactorial)


}
