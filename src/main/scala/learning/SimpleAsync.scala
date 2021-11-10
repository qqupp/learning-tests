package learning

import java.util.concurrent.Callable

import learning.SimpleAsync.ThreadAsyncExecutor

import scala.util.Try

object SimpleAsync {

  trait AsyncResult[T] {
    def isCompleted: Boolean
    def getValue: T
    def await: Unit
  }

  class CompletableResult[T](callback: Option[AsyncCallback[T]]) extends AsyncResult[T] {
    @volatile var result: Either[Throwable, T] = _

    def complete(t: Either[Throwable, T]): Unit = {
      result = t
      callback.fold(
        ()
      )(cb => cb.onComplete(t))
    }

    def isCompleted: Boolean = result != null

    def getValue: T = result.right.get

    def await: Unit = while (result == null) ()
  }

  trait AsyncCallback[T] {
    def onComplete(value: Either[Throwable, T]): Unit
  }

  trait AsyncExecutor {
    def startProccessWithCallback[T](callable: Callable[T], callback: Option[AsyncCallback[T]]): AsyncResult[T]
  }

  class ThreadAsyncExecutor extends AsyncExecutor {
    def startProcess[T](callable: Callable[T]): AsyncResult[T] =
      startProccessWithCallback(callable, None)

    def startProccessWithCallback[T](callable: Callable[T], callback: Option[AsyncCallback[T]]): AsyncResult[T] = {
      val result = new CompletableResult(callback)
      new Thread((new Runnable {
        def run(): Unit = result.complete(Try(callable.call()).toEither)
      })).start()
      result
    }

  }
}

object TestAsync extends App {

  val executor = new ThreadAsyncExecutor

  private def process[T](value: T, delayMillis: Long): Callable[T] =
    () => {
      def foo() = {
        Thread.sleep(delayMillis)
        println(s"process $value")
        value
      }

      foo()
    }

  val p1 = process(1, 5000)
  val p2 = process(2, 4000)
  val p3 = process(3, 3000)
  val p4 = process(4, 2000)
  val p5 = process(5, 1000)

  val results: List[SimpleAsync.AsyncResult[Int]] = List(
    executor.startProcess(p1),
    executor.startProcess(p2),
    executor.startProcess(p3),
    executor.startProccessWithCallback(p4, Some({ case Right(v) => println(s"Now you have finished $v"); case _ => })),
    executor.startProcess(p5)
  )

  println("continuing the program")

  val r1: SimpleAsync.AsyncResult[Int] = results(0)

  def showComplete(v: SimpleAsync.AsyncResult[Int]): Unit = {
    val r = if (v.isCompleted) s"Completed ${v.getValue}" else "NotCompleted"
    println(s"p1 Status:  ${r}")
  }

  showComplete(r1)
  r1.await
  showComplete(r1)

}
