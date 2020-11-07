package learning.typedmap

import learning.typedmap.TypedMap.Key
import org.scalatest.{AsyncFlatSpec, Matchers}
import scala.reflect.runtime.universe._

class TypedMapTest extends AsyncFlatSpec with Matchers {

  behavior of "TypedMap"

  val emptyMap: TypedMap[Unit] = TypedMap.empty

  val key1: Key[Unit, Int]             = Key[Int]
  val key2: Key[Unit, List[String]] = Key[List[String]]
  val key3: Key[Unit, List[Int]] = Key[List[Int]]
  def key4[T](implicit ev: TypeTag[T]): Key[Unit, List[T]] = Key.apply[List[T]]

  it should "return an empty map" in {
    val testEmptyMap: TypedMap[Unit] = TypedMap.empty
    val testKey1: Key[Unit, Int]     = Key[Int]
    testEmptyMap.get(testKey1) shouldBe None
  }

  it should "return a map for its type not modifying the original structure" in {
    emptyMap.updated(key1, 10).get(key1) shouldBe Some(10)

    emptyMap.get(key1) shouldBe None
  }

  it should "return the last updated value for its key" in {
    val result =
      emptyMap
        .updated(key1, 1)
        .updated(key1, 2)
        .get(key1)

    result shouldBe Some(2)
  }

  it should "test UnitKeyValue keys" in {
    key1 == key2 shouldBe false
    key2 == key3 shouldBe false
    key4[List[Int]] == key4[List[String]] shouldBe false
    key4[List[Set[Int]]] == key4[List[Set[Boolean]]] shouldBe false

    key3 == key4[Int] shouldBe true
    Key[Int] == Key[Int] shouldBe true
  }

  it should "test nonUnitKeyValue keys" in {
    sealed trait TestTypeKeys
    case object K1Value extends TestTypeKeys
    case class  KValue(n: Int) extends TestTypeKeys

    withClue("same keyValue and same type should be same key") {
      Key.of[TestTypeKeys, String](K1Value) == Key.of[TestTypeKeys, String](K1Value) shouldBe true
      Key.of[TestTypeKeys, String](KValue(3)) == Key.of[TestTypeKeys, String](KValue(3)) shouldBe true
    }

    withClue("different keyValue value and same type shouldBe different") {
      Key.of[TestTypeKeys, String](KValue(3)) == Key.of[TestTypeKeys, String](KValue(2)) shouldBe false
      Key.of[TestTypeKeys, String](K1Value) == Key.of[TestTypeKeys, String](KValue(1)) shouldBe false
    }

    withClue("same keyValue value and different type shouldBe different") {
      Key.of[TestTypeKeys, String](K1Value) == Key.of[TestTypeKeys, Boolean](K1Value) shouldBe false
      Key.of[TestTypeKeys, String](KValue(3)) == Key.of[TestTypeKeys, Boolean](KValue(3)) shouldBe false
    }

    sealed trait TestTypeKeys2
    case object K1Value2 extends TestTypeKeys2

    withClue("different keyValue type and same type shouldBe different") {
      Key.of[TestTypeKeys, Unit](K1Value) == Key.of[Unit, Unit](()) shouldBe false
      Key.of[TestTypeKeys, Unit](K1Value) == Key.of[TestTypeKeys2, Unit](K1Value2) shouldBe false
    }

    withClue("different keyValue type and different type shouldBe different") {
      Key.of[TestTypeKeys, String](K1Value) == Key.of[TestTypeKeys2, Int](K1Value2) shouldBe false
    }
  }

  it should "support key building with syntax" in {
    import Key._
    val testk1: Key[String, Int] = Key("testk1", WithType[Int])
    val testk2: Key[String, Boolean] = Key("testk1", WithType[Boolean])

    testk1 == testk2 shouldBe false

    Key(1, WithType[String]) == Key(2, WithType[String]) shouldBe false
    Key(1, WithType[String]) == Key(1, WithType[String]) shouldBe true
  }

  it should "differentiate nested hk types" in {
    val newFilters =
      emptyMap
        .updated(key2, List("a", "b"))
        .updated(key3, List(1, 2))

    newFilters.get(key2) shouldBe Some(List("a", "b"))
    newFilters.get(key3) shouldBe Some(List(1, 2))
  }

  it should "differentiate top level hk types" in {
    val newFilters =
      emptyMap
        .updated(key4[Boolean], List(false, true))
        .updated(key4[Int], List(1, 2))
        .updated(key4[List[Int]], List(List(1), List(3)))
        .updated(key4[Set[Boolean]], List(Set(false)))

    newFilters.get(key4[Boolean]) shouldBe Some(List(false, true))
    newFilters.get(key4[Int]) shouldBe Some(List(1, 2))
    newFilters.get(key4[List[Int]]) shouldBe Some(List(List(1), List(3)))
    newFilters.get(key4[Set[Boolean]]) shouldBe Some(List(Set(false)))
  }

  it should "be usable with deffent keysTypes" in {

    sealed trait TestKeyType
    case object T1        extends TestKeyType
    case object T2        extends TestKeyType
    case class T3(i: Int) extends TestKeyType

    val tmap: TypedMap[TestKeyType] = TypedMap.empty[TestKeyType]

    val keyFirst    = Key.of[TestKeyType, String](T1)
    val keySecond   = Key.of[TestKeyType, Boolean](T2)
    def key(n: Int) = Key.of[TestKeyType, Char](T3(n))

    val result =
      tmap
        .updated(keyFirst, "fst")
        .updated(keySecond, false)
        .updated(key(3), 't')
        .updated(key(4), 'f')

    result.get(keyFirst) shouldBe Some("fst")
    result.get(keySecond) shouldBe Some(false)
    result.get(key(3)) shouldBe Some('t')
    result.get(key(4)) shouldBe Some('f')
    result.get(key(5)) shouldBe None
  }

  it should "behave in example usage" in {
    import Key._

    object Keys {
      case class FilterKey(s: String)
      val id = Key(FilterKey("id"), WithType[String])
      val sorted = Key(FilterKey("sorted"), WithType[Boolean])
      val extra = Key(FilterKey("extra"), WithType[Boolean])
      val results = Key(FilterKey("results"), WithType[List[Int]])
      val others = Key(FilterKey("others"), WithType[List[Int]])
    }

    val emptyFilters = TypedMap.empty[Keys.FilterKey]

    val newFilters =
      emptyFilters
        .updated(Keys.extra, false)
        .updated(Keys.id, "id_string")
        .updated(Keys.sorted, true)
        .updated(Keys.results, List(20, 21, 22))

    newFilters.get(Keys.others) shouldBe None
    newFilters.get(Keys.id) shouldBe Some("id_string")
    newFilters.get(Keys.sorted) shouldBe Some(true)
    newFilters.get(Keys.results) shouldBe Some(List(20, 21, 22))
    newFilters.get(Keys.extra) shouldBe Some(false)
  }

}
