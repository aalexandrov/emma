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
package test

import compiler.MacroCompiler
import compiler.lang.opt.Reified

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Macros needed for the test cases.
 *
 * These are defined here and not in the `test` folder, because they need to be compiled
 * in an earlier compilation run.
 */
class TestMacros(val c: blackbox.Context) extends MacroCompiler {

  import Core.{Lang => core}
  import UniverseImplicits._

  val idPipeline: c.Expr[Any] => u.Tree =
    identity().compose(_.tree)

  def removeShadowedThisImpl[T](e: c.Expr[T]): c.Expr[T] = {
    //c.warning(e.tree.pos, "(1) " + c.universe.showCode(e.tree))
    val res = (idPipeline andThen removeShadowedThis andThen reTypeCheck) (e)
    //c.warning(e.tree.pos, "(2) " + c.universe.showCode(res))
    c.Expr(res)
  }

  def reifyImpl[C: c.WeakTypeTag, R: c.WeakTypeTag](e: c.Expr[R]): c.Expr[Reified[C, R]] = {
    //c.warning(e.tree.pos, "(1) " + c.universe.showCode(e.tree))
    val res = pipeline(withPost = false)(Core.lift, CoreUtils.enclose, showCode[C, R])(e.tree)
    //c.warning(e.tree.pos, "(2) " + c.universe.showCode(res))
    c.Expr[Reified[C, R]](res)
  }

  private lazy val reTypeCheck: u.Tree => u.Tree = tree =>
    typeCheck(unTypeCheck(tree))

  private def showCode[C: c.WeakTypeTag, R: c.WeakTypeTag]: u.Tree => u.Tree =
    tree => {

      val C = implicitly[u.WeakTypeTag[C]].tpe
      val R = implicitly[u.WeakTypeTag[R]].tpe

      val code = u.showCode(pipeline(withPre = false)()((tree match {
        case core.Let(Seq(core.ValDef(_, rhs), _), Seq(), _) =>
          Some(rhs) // vals.find(_.symbol == x).map(_.rhs)
        case _ =>
          None
      }) getOrElse tree))

      pipeline(withPre = false)()(
        core.DefCall(
          Some(core.Ref(Reified$API.sym)),
          Reified$API.apply,
          Seq(C, R), Seq(Seq(core.Lit(code)))))
    }

  object Reified$API extends ModuleAPI {
    //@formatter:off
    val sym   = api.Sym[Reified.type].asModule
    val apply = op("apply")
    val ops   = Set(apply)
    //@formatter:on
  }

}

object TestMacros {

  def removeShadowedThis[T](e: T): T = macro TestMacros.removeShadowedThisImpl[T]

  /** Transforms the quoted expression `e` into Emma Core. */
  def reify[C, R](e: R): Reified[C, R] = macro TestMacros.reifyImpl[C, R]

  /** Helper method -- enables type inference of enclosed [[Reified]] objects. */
  def enclose[C, R](c: C)(r: Reified[C, R]) = r
}
