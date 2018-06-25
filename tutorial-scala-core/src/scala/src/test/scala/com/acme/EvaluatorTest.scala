package com.acme

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * This is an implementation of _property-based_ testing approach, where the test
  * does not concern itself with the values, but with what should hold for all inputs
  * of the given type.
  */
class EvaluatorTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import Evaluator._
  import Expr._

  it should "evaluate simple expressions" in {
    // for all `BigDecimal`s; note that the test does not contain any specific values,
    // it relies on what the generator (the machinery in `forAll`) provides.
    forAll { n: BigDecimal ⇒
      // the following should hold...
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
