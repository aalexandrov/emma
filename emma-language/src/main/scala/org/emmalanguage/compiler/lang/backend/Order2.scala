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
import util.Memo
import util.Monoids._

import shapeless._

import collection.breakOut
import scala.annotation.tailrec
import scala.collection.BitSet

private[backend] trait Order2 extends Common {
  self: Core with ControlFlow with Comprehension =>

  import API._
  import Core.{Lang => core}
  import UniverseImplicits._

  type LevelMap = Map[u.TermSymbol, Short]
  type OrderMap = Map[u.TermSymbol, BitSet]
  type ReachMap = Map[u.TermSymbol, Vector[u.TermSymbol]]

  type DataGraph = quiver.Graph[u.TermSymbol, u.ValDef, Unit]

  private[backend] object Order2 {

    lazy val order: u.Tree => OrderMap = tree => {

      // compute the data- and control-flow graphs of the input `tree`
      val G = ControlFlow.cfg(tree)
      val D = G.data.tclose

      // initialize a memo table for level maps tied to the gurrent G graph
      val levelsMemo = Memo[Set[u.TermSymbol], LevelMap](levels(_)(D))

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

      val O = collection.mutable.Map[u.TermSymbol, BitSet]()

      // Case 1
      for (x <- D.nodes if !(E contains x))
        O += x -> BitSet(0) // `x` without enclosing `y`

      for {
        xSym <- D.nodes
        ySym <- E.get(xSym)
        yDef <- D.label(ySym)
        if O contains ySym // O(y) is defined
        if (yDef.rhs match { // Case 2: y is a Comprehension
          case cs.Comprehension(_, _) => true
          case _ => false
        })
      } yDef.rhs match {
        // Case 2: y is a Comprehension
        case cs.Comprehension(qs, _) =>
          val gs = generators(qs)
          val L = levelsMemo(gs)
          val X =
            if (gs(xSym)) L(xSym) // Case 2.a) x is a Generator
            else { // Case 2.b) x is a regular ValDef
              val zs = D.predecessors(xSym).filter(gs)
              max(for (z <- zs) yield L(z))
            }
          O += xSym -> O(ySym).map(_ + X)

        // Case 3: y is a Lambda TODO
        case core.Lambda(_, _, _) =>

        case _ =>
      }

      O.toMap
    }

    lazy val disambiguate: u.Tree => u.Tree = tree =>
      tree
  }

  private lazy val cs = Comprehension.Syntax(DataBag.sym)

  private def levels(gs: Set[u.TermSymbol])(C: DataGraph): LevelMap = {
    val fC = C.nfilter(gs)
    val ns = gs.map(g => g -> fC.predecessors(g).filterNot(_ == g))(breakOut): ReachMap
    enumLevels(ns, Map.empty[u.TermSymbol, Short])
  }

  @tailrec
  private def enumLevels(rem: ReachMap, lvl: LevelMap): LevelMap =
    if (rem.isEmpty) lvl
    else {
      val diff = rem.filter(_._2 forall lvl.keySet)
      enumLevels(
        rem -- diff.keySet,
        lvl ++ diff.mapValues(vs => (max(vs.map(lvl)) + 1).toShort))
    }

  @inline
  private def generators(qs: Seq[u.Tree]): Set[u.TermSymbol] =
    (qs collect { case cs.Generator(g, _) => g }) (breakOut)

  @inline
  private def max(vs: Vector[Short]): Short =
    vs.reduceOption(_ max _).getOrElse[Short](0)
}
