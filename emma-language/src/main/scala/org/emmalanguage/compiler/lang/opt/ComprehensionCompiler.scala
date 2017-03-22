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
package compiler.lang.opt

import compiler.Common
import compiler.lang.backend.Backend
import compiler.lang.comprehension.Comprehension
import compiler.lang.core.Core
import compiler.tools.GraphTools

import scala.collection.breakOut

trait ComprehensionCompiler extends Common
  with Backend
  with Comprehension
  with GraphTools {
  self: Core =>

  import Core.{Lang => core}
  import UniverseImplicits._

  object ComprehensionCompiler {

    lazy val cs = new Comprehension.Syntax(API.DataBag.sym)

    def apply[C, R](r: Reified[C, R])(e: C): R = {
      // parse the reified expression and desugar all DataBag comprehensions
      val rslt = time(pipeline(typeCheck = true)(
        ComprehensionCompiler.fixReified,
        ComprehensionCompiler.optimizeComprehension(asProduct(e)),
        ComprehensionCompiler.ensureArity1
      )(parse(r.lambda)), "rslt pipeline")
      // compile and evaluate the rslt function
      e match {
        case _: Unit => time(eval[() => R](rslt), "eval")()
        case _ => time(eval[C => R](rslt), "eval")(e)
      }
    }

    def time[A](f: => A, name: String = "") = {
      val s = System.nanoTime
      val ret = f
      println(s"$name time: ${(System.nanoTime - s) / 1e6}ms".trim)
      ret
    }

    // TODO: maybe integrate as part of the standard prepass fixes
    /** Fixes reified trees after parse. */
    private lazy val fixReified: u.Tree => u.Tree =
      api.BottomUp.transform({
        case dc@core.DefCall(t, m, targs, Seq(Seq(rhs)))
          if API.ComprehensionSyntax.ops contains m => rhs match {
          case _: u.Block => dc
          case _ => core.DefCall(t, m, targs, Seq(Seq(api.Block(expr = rhs))))
        }
      })._tree

    def optimizeComprehension(e: Product): u.Tree => u.Tree = tree => {
      for ((ps, c@cs.Comprehension(qs, hd)) <- extractComprehension(tree)) {
        val u = e
        val v = c
        val w = ps
        val x = qs
        val y = hd
        val k = e.productArity
        val l = ps.size
        val z = 1
      }
      Comprehension.desugar(API.DataBag.sym)(tree)
    }

    private lazy val extractComprehension: u.Tree => Option[(Seq[u.ValDef], u.Tree)] = {
      case core.Lambda(_, ps, core.Let(_ :+ core.ValDef(x, c@cs.Comprehension(_, _)), _, core.Ref(y)))
        if x == y => Some(ps, c)
      case core.Lambda(_, ps, core.Let(_, _, core.ValDef(_, c@cs.Comprehension(_, _)))) =>
        Some(ps, c)
      case _ =>
        None
    }

    /** If the tree type is a function, ensure that the its arity at most one. */
    private lazy val ensureArity1: u.Tree => u.Tree = tree => {
      val F = tree.tpe
      if (!(api.Sym.funs contains F.typeSymbol) || F.typeArgs.size < 3) tree
      else core.DefCall(Some(FnAPI.ref), FnAPI.tupled(F.typeArgs.size), F.typeArgs, Seq(Seq(tree)))
    }

    object FnAPI extends ModuleAPI {
      //@formatter:off
      lazy val sym              = api.Sym[scala.Function.type].asModule
      lazy val ops              = tupled.values.toSet
      lazy val tupled           =
        sym.info.member(api.TermName("tupled")).alternatives.map(a => {
          val k = a.typeSignature.typeParams.size
          val v = a.asMethod
          k -> v
        })(breakOut): Map[Int, u.MethodSymbol]
      //@formatter:on
    }

  }

  private def asProduct[A](a: A): Product = a match {
    case a: Product => a
    case _: Unit => NullaryProduct
    case _ => Tuple1(a)
  }

  private object NullaryProduct extends Product {
    def productElement(n: Int): Nothing =
      throw new IndexOutOfBoundsException("Nullary product does not have elements")

    def productArity = 0

    override def canEqual(that: Any) = that match {
      case _: NullaryProduct.type => true
      case _ => false
    }
  }

}
