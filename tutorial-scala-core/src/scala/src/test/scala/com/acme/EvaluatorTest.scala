package com.acme

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class EvaluatorTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import Evaluator._
  import Expr._

  it should "evaluate simple expressions" in {
    forAll { n: BigDecimal ⇒
      val Z = BigDecimal(0)
      eval(n plus Z) shouldBe Right(n)
      eval(Z plus n) shouldBe Right(n)
      eval(n minus n) shouldBe Right(Z)
      eval(n mult Z) shouldBe Right(Z)
      eval(Z mult n) shouldBe Right(Z)
      val Left(_) = eval(n div Z)
    }
  }

}
