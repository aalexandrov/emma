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

import compiler.Common
import compiler.lang.cf.ControlFlow
import compiler.lang.comprehension.Comprehension
import compiler.lang.core.Core
import util.Monoids._
import util.Memo

import shapeless._

import collection.breakOut
import scala.annotation.tailrec

private[backend] trait Order2 extends Common {
  self: Core with ControlFlow with Comprehension =>

  import API._
  import Core.{Lang => core}
  import UniverseImplicits._

  type LevelMap = Map[u.TermSymbol, Short]
  type OrderMap = Map[u.TermSymbol, Short]
  type ReachMap = Map[u.TermSymbol, Vector[u.TermSymbol]]

  type DataGraph = quiver.Graph[u.TermSymbol, u.ValDef, Unit]

  private[backend] object Order2 {

    lazy val order: u.Tree => OrderMap = tree => {

      // compute the data- and control-flow graphs of the input `tree`
      val G = ControlFlow.cfg(tree)
      val D = G.data.tclose

      // initialize a memo table for level maps tied to the gurrent G graph
      val levelsMemo = Memo[u.ValDef, LevelMap](levels(_)(D))

      // compute the 'enclosing symbol' map of the input `tree`
      val E = {
        val builder = Map.newBuilder[u.TermSymbol, u.TermSymbol]
        api.TopDown.inherit[Option[u.TermSymbol]] {
          case core.ValDef(lhs, core.Lambda(_, _, _)) => Some(lhs)
          case core.ValDef(lhs, cs.Comprehension(_, _)) => Some(lhs)
        }(last(None)).traverseWith {
          case Attr.inh(core.BindingDef(lhs, _), Some(encl) :: _) => builder += lhs -> encl
        }(tree)
        builder.result()
      }

      val O = collection.mutable.Map[u.TermSymbol, Short]()

      for (x <- D.nodes if !(E contains x))
        O += x -> 0 // `x` without enclosing `y`

      for {
        xSym <- D.nodes
        xDef <- D.label(xSym)
        ySym <- E.get(xSym)
        yDef <- D.label(ySym)
        if O contains ySym // O(y) is defined
        if (yDef.rhs match { // y is a Comprehension
          case cs.Comprehension(_, _) => true
          case _ => false
        })
      } {
        val L = levelsMemo(D.label(ySym).get)
        val X = xDef match {
          case cs.Generator(_, _) => // x is a Generator
            L(xSym)
          case _ => // x is a regular ValDef
            val gs = generators(yDef)
            D.predecessors(xSym).filter(gs).map(L).reduceOption(_ max _).getOrElse[Short](0)
        }
        O += xSym -> (O(ySym) + X).toShort
      }

      O.toMap
    }

    lazy val disambiguate: u.Tree => u.Tree = tree =>
      tree
  }

  private lazy val cs = Comprehension.Syntax(DataBag.sym)

  private def levels(vd: u.ValDef)(C: DataGraph): LevelMap = {
    val gs = generators(vd)
    val fC = C.nfilter(gs)
    val ns = gs.map(g => g -> fC.predecessors(g).filterNot(_ == g))(breakOut): ReachMap
    enum(ns)
  }

  @tailrec
  private def enum(rem: ReachMap, lvl: LevelMap = Map.empty[u.TermSymbol, Short]): LevelMap =
    if (rem.isEmpty) lvl
    else {
      val diff = rem.filter(_._2 forall lvl.keySet)
      enum(
        rem -- diff.keySet,
        lvl ++ diff.mapValues(vs => (vs.map(lvl).reduceOption(_ max _).getOrElse[Short](0) + 1).toShort))
    }

  private def generators(vd: u.ValDef): Set[u.TermSymbol] = {
    val core.ValDef(_, cs.Comprehension(qs, _)) = vd
    (qs collect { case cs.Generator(g, _) => g }) (breakOut): Set[u.TermSymbol]
  }
}
