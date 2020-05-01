package purecommands.semantic.evaluation.denotational

import org.scalatest.{FlatSpec, Matchers}
import purecommands.rts.{Env, FunEnv, FunStore, Store}
import purecommands.semantic.domain.Dval._
import purecommands.semantic.domain.Eval.EInt
import purecommands.semantic.domain.{Dval, Mval}
import purecommands.semantic.domain.Mval._
import purecommands.semantic.evaluation.denotational.Evaluation._
import purecommands.syntactic.domain.Exp._
import purecommands.syntactic.domain.Com._

class EvaluationTest extends FlatSpec with Matchers {

  "sem" should "produce eval expr" in {

    val rho: Env[Dval] = FunEnv.emptyEnv(Unbound)
    val sigma: Store[Int, Mval] = FunStore.emptyStore(Undefined)

    val (loc1, sigma1) = sigma.allocate(MInt(5))
    val rho1 = rho.bind("x", DLoc(loc1))
    val (loc2, sigma2) = sigma1.allocate(MInt(1))
    val rho2 = rho1.bind("acc", DLoc(loc2))


    sem (Val(Den("x")))(rho2)(sigma2) shouldBe EInt(5)
    sem (Val(Den("acc")))(rho2)(sigma2) shouldBe EInt(1)

    val program = List(
      While(
        Not(Eq(
          Val(Den("x")),
          LitteralInt(0)
          )),
        List(
          Assign(
            Den("acc"), Prod(Val(Den("acc")), Val(Den("x")))
          ),
          Assign(
            Den("x"), Diff(Val(Den("x")), LitteralInt(1)))
          )
        )
      )

    val sigma3 = semComs(program)(rho2)(sigma2)

    sem (Val(Den("x")))(rho2)(sigma3) shouldBe EInt(0)
    sem (Val(Den("acc")))(rho2)(sigma3) shouldBe EInt(5 * 4 * 3 * 2 * 1)

  }
}
