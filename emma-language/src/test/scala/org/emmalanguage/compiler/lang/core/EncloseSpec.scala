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
package compiler.lang.core

import api._
import compiler.BaseCompilerSpec

/** A spec for the `CoreUtils.enclose` transformation. */
class EncloseSpec extends BaseCompilerSpec {

  import compiler._

  val typecheck: u.Expr[Any] => u.Tree =
    expr => compiler.typeCheck(expr.tree)

  // use `compiler.pipeline` instead of `pipeline` to bypass `checkCompile`, as
  // compiling the output causes runtime errors in some tests
  val lnfPipeline: u.Tree => u.Tree =
    compiler.pipeline()(
      Core.lnf
    )

  // use `compiler.pipeline` instead of `pipeline` to bypass `checkCompile`, as
  // compiling the output causes runtime errors in some tests
  val enclosePipeline: u.Tree => u.Tree =
    compiler.pipeline()(
      Core.lnf,
      tree => time(CoreUtils.enclose(tree), "enclose")
    )

  "creates nullary lambda if closure is empty" in {
    val act = enclosePipeline(typecheck(u.reify {
      val xs = DataBag(Seq(1, 2, 3, 4, 5))
      val ys = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)
      ys union xs.map(_ + Math.PI)
    }))

    val exp = lnfPipeline(typecheck(u.reify {
      val f = () => {
        val xs = DataBag(Seq(1, 2, 3, 4, 5))
        val ys = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)
        ys union xs.map(_ + Math.PI)
      }
      f()
    }))

    act shouldBe alphaEqTo(exp)
  }

  "references to out-of-quote value definitions" - {
    "as free symbols" in {
      val xs = DataBag(Seq(1, 2, 3, 4, 5))
      val ys = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)

      val act = enclosePipeline(typecheck(u.reify {
        ys union xs.map(_ + Math.PI)
      }))

      val exp = lnfPipeline(typecheck(u.reify {
        val f = (xs: DataBag[Int], ys: DataBag[Double]) =>
          ys union xs.map(_ + Math.PI)
        f(xs, ys)
      }))

      act shouldBe alphaEqTo(exp)
    }

    "as bound symbols" in {
      val snippet = typecheck(u.reify {
        val xs = DataBag(Seq(1, 2, 3, 4, 5))
        val ys = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)

        val act = {
          ys union xs.map(_ + Math.PI)
        }

        val exp = {
          val f = (xs: DataBag[Int], ys: DataBag[Double]) =>
            ys union xs.map(_ + Math.PI)
          f(xs, ys)
        }
      })

      val act = enclosePipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("act") => rhs
      }).head)

      val exp = lnfPipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("exp") => rhs
      }).head)

      act shouldBe alphaEqTo(exp)
    }
  }

  "`this` references for enclosing classes" - {
    "as free symbols" in {
      val act = enclosePipeline(typecheck(u.reify {
        ys.map(_ * Math.E) union xs.map(_ + Math.PI)
      }))

      val exp = lnfPipeline(typecheck(u.reify {
        val f = (this$: EncloseSpec) => {
          this$.ys.map(_ * Math.E) union this$.xs.map(_ + Math.PI)
        }
        f(this)
      }))

      act shouldBe alphaEqTo(exp)
    }

    "as bound class symbols" in {
      val snippet = typecheck(u.reify {
        class Foo {
          val us = DataBag(Seq(1, 2, 3, 4, 5))
          val vs = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)

          val act = {
            vs union us.map(_ + Math.PI)
          }

          val exp = {
            val f = (this$: Foo) => {
              this$.vs union this$.us.map(_ + Math.PI)
            }
            f(this)
          }
        }
      })

      val act = enclosePipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("act") => rhs
      }).head)

      val exp = lnfPipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("exp") => rhs
      }).head)

      act shouldBe alphaEqTo(exp)
    }

    "as bound object symbols" in {
      val snippet = typecheck(u.reify {
        object Foo {
          val us = DataBag(Seq(1, 2, 3, 4, 5))
          val vs = DataBag(Seq(1, 2, 3, 4, 5)).map(_ * 1.0)

          val act = {
            vs union us.map(_ + Math.PI)
          }

          val exp = {
            val f = () => {
              Foo.vs union Foo.us.map(_ + Math.PI)
            }
            f()
          }
        }
      })

      val act = enclosePipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("act") => rhs
      }).head)

      val exp = lnfPipeline(snippet.collect({
        case u.ValDef(_, nme, _, rhs)
          if nme.toString.startsWith("exp") => rhs
      }).head)

      act shouldBe alphaEqTo(exp)
    }
  }
}
