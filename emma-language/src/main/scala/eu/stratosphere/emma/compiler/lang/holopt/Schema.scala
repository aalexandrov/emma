package eu.stratosphere.emma
package compiler.lang.holopt

import compiler.lang.core.Core
import compiler.lang.comprehension.Comprehension
import compiler.Common
import util.Monoids

import shapeless._

import scala.annotation.tailrec

trait Schema extends Common {
  this: Core with Comprehension =>

  import UniverseImplicits._
  import Core.{Lang=>core}

  private[holopt] object Schema {

    val cs = new Comprehension.Syntax(API.bagSymbol)

    lazy val analyze: u.Tree => Info = tree => {

      val elementOf = Map.newBuilder[u.TermSymbol, u.TermSymbol]

      api.BottomUp.traverse {
        case core.ValDef(bag, cs.Comprehension(qs, cs.Head(core.Let(_, _, core.Ref(elm)))), _) =>
          elementOf += elm -> bag
        case cs.Generator(elm, core.Let(_, _, core.Ref(bag))) =>
          elementOf += bag -> elm
      }(tree)

      val res = Info(elementOf.result())
      res
    }

    lazy val normalize: u.Tree => u.Tree = tree => {
      tree
    }

    //@formatter:off
    case class Info
    (
      elementOf: Map[u.TermSymbol, u.TermSymbol]
    )
    //@formatter:on
  }

}
