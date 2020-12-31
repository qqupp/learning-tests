package learning

import org.scalatest.{FlatSpec, Matchers}

class HasTypeclassTest extends FlatSpec with Matchers {

  "has" should "give left" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = (1, 'a')
    val res     = has[Int](testVal)

    res shouldBe List(1)
  }

  it should "give right" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = (1, 'a')
    val res     = has[Char](testVal)

    res shouldBe List('a')
  }

  it should "give left right" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = (1, 2)
    val res     = has[Int](testVal)

    res shouldBe List(1, 2)
  }

  it should "give nested" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = ((1, (false, "aaa")), ('c', 4.0))
    val resD    = has[Double](testVal)
    val resS    = has[String](testVal)
    val resI    = has[Int](testVal)

    resD shouldBe List(4.0)
    resS shouldBe List("aaa")
    resI shouldBe List(1)
  }

  it should "merge nested balanced" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = ((3, 1), ((6, 7), (4, 5)))
    val resI    = has[Int](testVal)
    var resII   = has[(Int, Int)](testVal)

    resI shouldBe List(3, 1, 6, 7, 4, 5)
    resII shouldBe List((3, 1), (6, 7), (4, 5))
  }

  it should "merge nested unbalanced" in {
    import HasTypeclass.Tuple2HasInstances._
    import HasTypeclass._
    val testVal = (((3, 2), 1), (6, (4, 5)))
    val resI    = has[Int](testVal)
    val resII   = has[(Int, Int)](testVal)

    resI shouldBe List(3, 2, 1, 6, 4, 5)
    resII shouldBe List((3, 2), (4, 5))
  }

}
