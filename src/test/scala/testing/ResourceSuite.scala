package testing

import cats.effect.concurrent.Deferred
import cats.effect.{IO, Resource}
import org.scalatest.BeforeAndAfterAll

trait ResourceSuite[A] extends PureTestSuite with BeforeAndAfterAll {

  def resource: Resource[IO, A]

  def withResource(f: (=> A) => Unit): Unit = f {
    latch.get.unsafeRunSync
    res
  }

  private[this] var res: A = _
  private[this] var cleanUp: IO[Unit] = _
  private[this] val latch = Deferred[IO, Unit].unsafeRunSync()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val (r, h) = resource.allocated.unsafeRunSync()
    res = r
    cleanUp = h
    latch.complete(()).unsafeRunSync()
  }

  override def afterAll(): Unit = {
    cleanUp.unsafeRunSync()
    super.afterAll()
  }
}