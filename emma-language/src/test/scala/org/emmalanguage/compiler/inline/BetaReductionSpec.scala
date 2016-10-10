/*
 * Copyright © 2014 TU Berlin (emma@dima.tu-berlin.de)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.emmalanguage
package compiler.inline

import compiler.BaseCompilerSpec

import lib.linalg._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/** A spec for the `Beta.reduce` transformation. */
@RunWith(classOf[JUnitRunner])
class BetaReductionSpec extends BaseCompilerSpec {

  import compiler._

  val liftPipeline: u.Expr[Any] => u.Tree =
    pipeline(typeCheck = true)(
      Core.lift
    ) andThen unQualifyStatics compose (_.tree)

  val prettyPrint: u.Tree => String =
    tree => time(Core.prettyPrint(tree), "pretty print")

  // ---------------------------------------------------------------------------
  // Example A: Program closure
  // ---------------------------------------------------------------------------

  val μ = 3.14
  val ν = 42

  // ---------------------------------------------------------------------------
  // Example A: various program representations
  // ---------------------------------------------------------------------------

  lazy val `Example A (Scala Expr)` =
    u.reify {
      val check = (e: Double) => e < ν
      val x = 51L
      val y = 17
      val e = plus(times(x, y), square(μ))
      val r = check(e)
      r
    }

  lazy val `Example A (Emma Source)` =
    idPipeline(u.reify {
      val check = (e: Double) => e < ν
      val x = 51L
      val y = 17
      val e = plus.apply[Double](
        times[Long](x, y.toLong)(Numeric.LongIsIntegral).toDouble,
        square[Double](μ)(Numeric.DoubleIsFractional)
      )(Numeric.DoubleIsFractional)
      val r = check(e)
      r
    })

  lazy val `Example A (Emma Core)` =
    liftPipeline(`Example A (Scala Expr)`)

  // ---------------------------------------------------------------------------
  // Spec
  // ---------------------------------------------------------------------------

  "Alpha-equivalent definition for " - {

    "`Example A (Emma Source)`" in {
      idPipeline(`Example A (Scala Expr)`) shouldBe alphaEqTo(`Example A (Emma Source)`)
    }
  }
}
