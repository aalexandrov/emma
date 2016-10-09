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
package api.emma

import compiler.MacroCompiler

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("Enable macro paradise to expand macro annotations.")
class lib extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro libMacro.inlineImpl

}

private class libMacro(val c: whitebox.Context) extends MacroCompiler {

  import c.universe._

  /** The implementation of the @emma.inline macro.
   *
   * 1. If the annottee is an object, all methods on that object will be extended
   * 2. If the annottee is a method, only the single method will be extended
   *
   * @param annottees a list with (currently) exactly one element (only objects and methods can be currently annotated)
   * @return the resulting tree with the replacement
   */
  def inlineImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    ensure(annottees.size == 1, "Only one object can be annotated as libmod.")

    val annottee = annottees.head
    c.Expr(transformModule(annottee.tree))
  }

  lazy val transformModule: Tree => Tree = {
    case tree@ModuleDef(mods, name, Template(parents, self, body)) =>
      val res = ModuleDef(mods, name, Template(parents, self, body map transformDefDef))
      // c.warning(tree.pos, c.universe.showCode(res))
      res
    case tree: Tree =>
      // not a module: issue a warning and return
      c.warning(tree.pos, "Unexpected non-module annottee found for `libmod` annotation.")
      tree
  }

  /**
   * Transform a DefDef tree.
   *
   * If the DefDef is annotated with `@emma.libfun`, this transformation will remove this
   * annotation and attach the serialized code associated with the DefDef instead.
   *
   * For example
   *
   * {{{
   * @emma.libfun
   * def add(x: Int, y: Int) = x + y
   * }}}
   *
   * will become
   *
   * {{{
   * @ast("def add(x: Int, y: Int) = x + y")
   * def add(x: Int, y: Int) = x + y
   * }}}
   */
  lazy val transformDefDef: Tree => Tree = {
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) if name == api.TermName.app =>
      // clear all existing annotations from the DefDef
      val clrDefDef = DefDef(mods mapAnnotations (_ => List.empty[Tree]), name, tparams, vparamss, tpt, rhs)
      // construct a string literal for the code of the cleared DefDef
      val litDefDef = codeLiteralFor(identity(typeCheck = true)(clrDefDef))
      DefDef(mods mapAnnotations append(src(litDefDef)), name, tparams, vparamss, tpt, rhs)

    case tree =>
      tree
  }

  val srcSym = rootMirror.staticClass("org.emmalanguage.api.emma.src")

  /** Construct the AST for `@emma.libfun($lit)` annotation. */
  def src(lit: Literal): Tree =
    api.Inst(srcSym.toTypeConstructor)(Seq(lit))

  /** Show the code for the given Tree and wrap it as a string literal. */
  def codeLiteralFor(tree: Tree): Literal =
    api.Lit(c.universe.showCode(tree))

  /** Append an annotation `ann` to an `annotations` list. */
  def append(ann: Tree)(annotations: List[Tree]): List[Tree] =
    annotations :+ ann

  /** Ensure a condition applies or exit with the given error message. */
  def ensure(condition: Boolean, message: => String): Unit =
    if (!condition) c.error(c.enclosingPosition, message)
}

