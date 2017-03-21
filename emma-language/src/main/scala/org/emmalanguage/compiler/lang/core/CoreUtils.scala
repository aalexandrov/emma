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

import compiler.Common
import compiler.lang.cf.ControlFlow

import shapeless._

import scala.collection.breakOut

/** Utility transformations for the Core language. */
private[core] trait CoreUtils extends Common {
  self: Core with ControlFlow =>

  import Core.{Lang => core}
  import UniverseImplicits._
  import u.addFlagOps

  private[emmalanguage] object CoreUtils {

    lazy val paramFlags = u.Flag.SYNTHETIC | u.Flag.PARAM

    /**
     * Explicates the closure of an LNF term as a lambda and a subsequent call.
     *
     * == Preconditions ==
     * - The input tree is in LNF or Emma Core (see [[Core.lnf]]and [[Core.lift]]).
     *
     * == Postconditions ==
     * - Out-of-scope references are substituted as lambda parameters.
     */
    lazy val enclose: u.Tree => u.Tree = tree => {
      val own = api.Owner.of(tree)
      val kvm: Map[u.Symbol, u.TermSymbol] =
        (thiss(tree) ++ api.Tree.closure(tree)).map({
          case s: u.TermSymbol =>
            s -> api.TermSym(own, api.TermName(s.name), s.info, paramFlags, s.pos)
          case s: u.ClassSymbol =>
            s -> api.TermSym(own, api.TermName("tref"), s.toType, paramFlags)
        })(breakOut)
      val kvs = kvm.toList.sortBy(_._2.name.toString)

      api.BottomUp.withRoot.transformWith({
        case Attr.none(core.Ref(sym))
          if kvm contains sym => core.ParRef(kvm(sym))

        case Attr.none(core.This(sym)) =>
          if (kvm contains sym) core.ParRef(kvm(sym))
          else core.Ref(sym.asClass.selfType.termSymbol.asTerm)

        case Attr.inh(root, None :: _) =>
          val (fEncRef, fEncDef) = valRefAndDef(own, "fEnc",
            core.Lambda(kvs.map(_._2), root))

          val (fResRef, fResDef) = valRefAndDef(own, "fRes",
            core.DefCall(
              Some(fEncRef),
              fEncRef.tpe.member(api.TermName.app).asMethod,
              Seq(), Seq(kvs.map(_._1 match {
                case s: u.TermSymbol => core.Ref(s)
                case s: u.ClassSymbol => core.This(s)
              }))))

          core.Let(
            Seq(fEncDef, fResDef),
            Seq(),
            fResRef)
      })._tree(tree)
    }

    /** Creates a ValDef, and returns its Ident on the left hand side. */
    private def valRefAndDef(own: u.Symbol, name: String, rhs: u.Tree): (u.Ident, u.ValDef) = {
      val lhs = api.ValSym(own, api.TermName.fresh(name), rhs.tpe)
      (core.Ref(lhs), core.ValDef(lhs, rhs))
    }

    /** Returns a set of all this references to outer classes in `tree`. */
    private def thiss(tree: u.Tree): Set[u.Symbol] = tree.collect {
      case core.This(target) if !target.isModuleClass => target
    }.toSet
  }

}
