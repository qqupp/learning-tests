package learning.typedmap

import org.scalatest.{FlatSpec, Matchers}
import Env._

import scala.reflect.runtime.universe

class EnvTest extends FlatSpec with Matchers {
  it should "be correctly typed" in {
    val env: Env[Has[Int]] = Env(Tpe[Int], 1)

    env.get(Tpe[Int]).value shouldBe 1
  }

  it should "accumulate type with `with Has`" in {
    val env: Env[Has[Int] with Has[Boolean]] = Env(Tpe[Int], 1).add(Tpe[Boolean], false)

    env.get(Tpe[Int]).value shouldBe 1
    env.get(Tpe[Boolean]).value shouldBe false
  }


  it should "work with type aliases FIXTHIS" in {
    ignore
//    //No TypeTag available for MyAlias
//    type  MyAlias = Option[List[Int]]
//    val aliased: MyAlias = Some(List(10))
//    val env: Env[Has[Int] with Has[MyAlias]] = Env(Tpe[Int], 1).add(Tpe[MyAlias], aliased)
//
//    env.get(Tpe[MyAlias]).value shouldBe Some(List(10))
  }

  it should "passable as argument" in {

    def myfun(e: Env[Has[String]]): Int = e.get(Tpe[String]).value.length

    val e: Env[Has[Int] with Has[Boolean] with Has[Option[Int]] with Has[String] with Has[List[Boolean]]] =
      Env(Tpe[Int], 10)
        .add(Tpe[Boolean],false)
        .add(Tpe[Option[Int]],Option(22))
        .add(Tpe[String],"testValue")
        .add(Tpe[List[Boolean]],List(false))

    myfun(e) shouldBe "testValue".length

  }




}
