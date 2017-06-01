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
package compiler.lang.backend

import api._
import compiler.BaseCompilerSpec
import compiler.ir.ComprehensionSyntax._
import compiler.ir.DSCFAnnotations._
import test.schema.NLP._

import collection.breakOut
import scala.collection.BitSet

/** A spec for order disambiguation. */
//noinspection ScalaUnusedSymbol
class Order2Spec extends BaseCompilerSpec {

  import compiler._
  import u.reify

  //----------------------------------------------------------------------------
  // Test Helper Methods
  //----------------------------------------------------------------------------

  val anfPipeline: u.Expr[Any] => u.Tree =
    compiler.pipeline(typeCheck = true, withPost = false)(Core.anf).compose(_.tree)

  val liftPipeline: u.Expr[Any] => u.Tree =
    compiler.pipeline(typeCheck = true, withPost = false)(Core.lift).compose(_.tree)

  val order: u.Tree => Map[u.TermSymbol, BitSet] = tree =>
    time(Order2.order(tree), "order")

  val symbols: u.Tree => Map[String, u.TermSymbol] = tree =>
    ControlFlow.cfg(tree).data.nodes.map(s => s.name.toString -> s)(breakOut)

  //----------------------------------------------------------------------------
  // Test Data
  //----------------------------------------------------------------------------

  // Seahorse Valley
  val (cRe, cIm) = (-0.75, 0.1)
  val N = 1000
  val f = 0.01
  val ps = DataBag(-N to N)
  val zoom1 = (x: Int) => DataBag(-x to x).map(_ / f)
  val zoom2 = (x: Int, f: Double) => DataBag(-x to x).map(_ / f)

  "driver with control flow" in {
    val snippet = anfPipeline(reify {
      val cRe = this.cRe
      val cIm = this.cIm

      @whileLoop def while$r1(i: Int, zIm: Double, zRe: Double): (Double, Double) = {
        val cond = i < 100
        @loopBody def body$r1(): (Double, Double) = {
          val rSq = zRe * zRe
          val iSq = zIm * zIm
          val sRe = rSq - iSq
          val zRI = zRe * zIm
          val sIm = 2 * zRI
          val zRe$r2 = sRe + cRe
          val zIm$r2 = sIm + cIm
          val i$r2 = i + 1
          while$r1(i$r2, zIm$r2, zRe$r2)
        }
        @suffix def suffix$r1(): (Double, Double) = {
          val res = (zRe, zIm)
          res
        }

        if (cond) body$r1() else suffix$r1()
      }

      while$r1(0, 0.0, 0.0)
    })

    val O = order(snippet)
    val S = symbols(snippet)

    for (field <- Seq("cRe", "cIm", "i", "zIm", "zRe", "cond", "rSq", "iSq", "res"))
      withClue(s"O($field):")(O(S(field)) shouldBe BitSet(0))
  }

  "comprehension with dependent generators" - {
    "#1" in {
      val snippet = liftPipeline(reify {
        val xs = for {
          d <- {
            docs
          }
          s <- {
            val x1 = d.text
            val x2 = doc2sentences(x1)
            x2
          }
          p <- {
            val x3 = d.lang
            val x4 = s.text
            val x5 = maleNames(x3)(x4)
            x5
          }
          c <- {
            val x6 = s.text
            val x7 = companyNames(x6)
            x7
          }
        } yield {
          val x8 = (p, c)
          x8
        }
        xs
      })

      val O = order(snippet)
      val S = symbols(snippet)


      for (field <- Seq("xs"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(0))
      for (field <- Seq("d", "x1", "x2", "x3"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(1))
      for (field <- Seq("s", "x4", "x5", "x6", "x7"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(2))
      for (field <- Seq("p", "c", "x8"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(3))
    }

    "#2" in {
      val snippet = liftPipeline(reify {
        val ps = this.ps
        val ys = comprehension[Double, DataBag] {
          val x = generator[Int, DataBag] {
            ps
          }
          val y = generator[Int, DataBag] {
            ps
          }
          val u = generator[Double, DataBag] {
            val z1 = zoom1(x)
            z1
          }
          val v = generator[Double, DataBag] {
            val z2 = zoom2(y, u)
            z2
          }
          head[Double] {
            val w = u * v
            w
          }
        }
        ys
      })

      val O = order(snippet)
      val S = symbols(snippet)

      for (field <- Seq("ps", "ys"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(0))
      for (field <- Seq("x", "y", "z1"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(1))
      for (field <- Seq("u", "z2"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(2))
      for (field <- Seq("v", "w"))
        withClue(s"O($field):")(O(S(field)) shouldBe BitSet(3))
    }
  }

  "ambiguous lambda" ignore {
    val snippet = liftPipeline(reify {
      val f = (x: Int) => x + 1
      val a = f(6)
      val b = xs
      b.map(v => f(v)).forall(_ < a)
    })

    val O = order(snippet)
    val S = symbols(snippet)

    for (field <- Seq("a", "b", "f"))
      withClue(s"O($field):")(O(S(field)) shouldBe BitSet(0))
    for (field <- Seq("x"))
      withClue(s"O($field):")(O(S(field)) shouldBe BitSet(0, 1))
  }

  "enclosed in lambda" - {
    "assigned to parameter" ignore {
      val snippet = liftPipeline(reify {
        val f1 = (x: Int) => x + 1
        val f2 = (f: Int => Int) => f(2) * 7
        val x1 = f2(f1)
        x1
      })
    }
  }
}
