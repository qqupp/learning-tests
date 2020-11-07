package learning

object RecToIter extends App {

  class MutableStack[T] {

    private trait S
    private case class Push(top: T, rest: S) extends S
    private case object Empty extends S
    private var stack: S =  Empty

    def push(t: T): Unit = stack = Push(t, stack)
    def pop: T = stack match {
      case Push(t, rest) =>
        stack = rest
        t
      case _ => throw new RuntimeException()
    }

    def isEmpty: Boolean = stack match {
      case Empty => true
      case _ => false
    }

    def notEmpty: Boolean = !isEmpty

    def peek: T = stack match {
      case Push(t, _) => t
      case _ => throw new RuntimeException()
    }
  }

  def recFactorial(n: Int): Int =
    if (n == 0) 1 else n * recFactorial(n -1)

  /*
    Execution example for fact 4

    call fact(4)
    --
    ret 39
    actual n = 4
    evalbody n * fact(n -1)
    4 * call fact(3)
    --
    ret 44
    actual n = 3
    evalBody n * fact(n -1)
    3 * call fact(2)
    --
    ret 49
    actual n = 2
    evalbody n * fact(n -1)
    2 * call fact(1)
    --
    ret 54
    actual n = 1
    evalBody n * fact(n -1)
    1 * call fact(0)
    --
    ret 59
    actual n = 0
    evalBody 1
    1
   */



  def iterFactorial_SingleStack(n: Int): Int = {

    trait Label
    case class EvalActualParam(actualN: Int) extends Label
    case class KontFrame(k: Int => Int) extends Label
    case class Return(r: Int) extends Label
    case class ReturnUnit() extends Label

    val stack: MutableStack[Label] = new MutableStack()
    var returnValue: Int = -1

    stack.push(ReturnUnit())
    stack.push(EvalActualParam(n))

    while (stack.notEmpty) {
      val current = stack.pop

      current match  {
        case EvalActualParam(x) =>
          if (x == 0)
            stack.push(Return(1))
          else {
            stack.push(KontFrame(y =>  x * y))
            stack.push(EvalActualParam( x - 1 ))
          }

        case Return(x) =>
          val next = stack.pop
          next match {
            case KontFrame(k) => stack.push(Return(k(x)))
            case ReturnUnit() => returnValue = x
            case _ => stack.push(next)
          }

        case _ =>
      }
    }

    returnValue
  }


  def iterFactorial_doubleStack(n: Int): Int = {
    //if (n == 0) 1 else mul(n, recFactorial(n -1))

    trait Label
    case class EvalActualParam(actualN: Int) extends Label
    case class KontFrame(k: Int => Int) extends Label
    case class Return() extends Label

    val stack: MutableStack[Label] = new MutableStack()
    val tmpStack: MutableStack[Int] = new MutableStack()
    var returnValue: Int = -1



    stack.push(Return())
    stack.push(EvalActualParam(n))

    while (stack.notEmpty) {
      val current = stack.pop

      current match  {
        case EvalActualParam(x) =>
          if (x == 0)
            tmpStack.push(1)
          else {
            stack.push(KontFrame(y =>  x * y))
            stack.push(EvalActualParam( x - 1 ))
          }

        case KontFrame(k) =>
          val x = tmpStack.pop
          tmpStack.push(k(x))

        case Return() =>
          returnValue = tmpStack.pop
      }
    }

    returnValue


  }

  def recFib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case 2 => 1
    case _ => recFib(n - 2) + recFib(n - 1)
  }



  def iterFib(n: Int): Int = {

    trait Label
    case class Eval(x: Int) extends Label
    case class Kont(k: Int => Int => Int) extends Label

    val stack: MutableStack[Label] = new MutableStack()
    var retValue: Int = -1

    val argStack: MutableStack[Int] = new MutableStack()
    def Return(x: Int) = argStack.push(x)

    stack.push(Eval(n))

    while (stack.notEmpty) {
      stack.pop match {
        case Eval(x) =>
            println(s"Eval $x")
            x match {
              case 0 => Return(0)
              case 1 => Return(1)
              case 2 => Return(1)
              case _ =>
                stack.push(Kont(x1 => x2 => x1 + x2))
                stack.push(Eval(x - 1))
                stack.push(Eval(x - 2))
            }

        case Kont(f) =>
          val arg2 = argStack.pop
          val arg1 = argStack.pop
          println(s"Kont $arg1 $arg2")
          Return(f(arg1)(arg2))

      }
    }

    retValue = argStack.pop
    retValue
  }


  def recSort(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x::xs =>
      val (lt, gt) = xs.partition(y => y <= x)
      recSort(lt) ++ (x :: recSort(gt))
  }


  def iterSort(l: List[Int]): List[Int] = {

    trait Label
    case class Eval(arg: List[Int]) extends Label
    case class Kont(f: List[Int] => List[Int] => List[Int]) extends Label

    val stack: MutableStack[Label] = new MutableStack()
    val tmpStack: MutableStack[List[Int]] = new MutableStack()

    stack.push(Eval(l))

    while (stack.notEmpty) {
      stack.pop match {
        case Eval(actualList) =>
          actualList match {
            case Nil => tmpStack.push(Nil)
            case x::xs =>
              val (lt, gt) = xs.partition(y => y <= x)
              stack.push(Kont(llt => lgt => llt ++ (x :: lgt)))
              stack.push(Eval(lt))
              stack.push(Eval(gt))
          }
        case Kont(f) =>
          val lt = tmpStack.pop
          val gt = tmpStack.pop
          tmpStack.push(f(lt)(gt))
      }
    }

    tmpStack.pop

  }



  def iterSort_Noclosureavailable(l: List[Int]): List[Int] = {

    trait Label
    case class Eval(arg: List[Int]) extends Label
    case class Kont(pivot: Int) extends Label

    val stack: MutableStack[Label] = new MutableStack()
    val tmpStack: MutableStack[List[Int]] = new MutableStack()

    stack.push(Eval(l))

    while (stack.notEmpty) {
      stack.pop match {
        case Eval(actualList) =>
          actualList match {
            case Nil => tmpStack.push(Nil)
            case x::xs =>
              val (lt, gt) = xs.partition(y => y <= x)
              stack.push(Kont(x))
              stack.push(Eval(lt))
              stack.push(Eval(gt))
          }
        case Kont(piv) =>
          val lt = tmpStack.pop
          val gt = tmpStack.pop
          tmpStack.push(lt ++ ( piv :: gt))
      }
    }

    tmpStack.pop

  }

}
