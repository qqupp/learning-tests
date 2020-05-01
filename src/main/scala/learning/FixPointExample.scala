package learning

object FixPointExample {

  def recursiveFactorial(x: Int): Int = if (x == 0) 1 else x * recursiveFactorial(x -1)

  val gammaFactorial: (Int => Int) => Int => Int =
    (f: Int => Int) => (x: Int) =>  if (x == 0) 1 else x * f(x -1)

  def fix[A, B](f: (A => B) => A => B): A => B  =
    x => f(fix(f))(x)

  val factorial = fix(gammaFactorial)


}
