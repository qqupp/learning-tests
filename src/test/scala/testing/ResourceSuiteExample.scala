package testing

import cats.effect.{IO, Resource}
import org.scalatest.Matchers

class ResourceSuiteExample extends ResourceSuite[Int] with Matchers {

  override def resource: Resource[IO, Int] =
    Resource.make(IO{ println("Getting Resource"); 1})(i => IO{println(s"Releasing Resource $i")})

  withResource { intResource =>

    spec("Test resource example") {
      IO(intResource).map(_ shouldBe 1)
    }

    spec("Test resource 2") {
      IO.unit.map(_ => fail())
    }

  }
}
