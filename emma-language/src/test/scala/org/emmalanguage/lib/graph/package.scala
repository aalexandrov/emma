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
package lib.graph

import org.emmalanguage.api._

import scala.Ordering.Implicits._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import model._

// -----------------------------------------------------------------------------
// Data model
// -----------------------------------------------------------------------------

object model {

  val maxIterations = 50

  case class Node[ID: Ordering](id: ID)

  case class Edge[ID: Ordering](src: ID, tgt: ID)

  case class LNode[ID: Ordering, L](@emma.pk id: ID, label: Option[L])

  case class LEdge[ID: Ordering, L](@emma.pk src: ID, @emma.pk tgt: ID, label: Option[L])

  //@formatter:off
  implicit def nodeMeta[ID : Ordering : Meta] = new Meta[Node[ID]] {
    override def ctag: ClassTag[Node[ID]] = implicitly[ClassTag[Node[ID]]]
    override def ttag: TypeTag[Node[ID]] = implicitly[TypeTag[Node[ID]]]
  }

  implicit def edgeMeta[ID : Ordering : Meta] = new Meta[Edge[ID]] {
    override def ctag: ClassTag[Edge[ID]] = implicitly[ClassTag[Edge[ID]]]
    override def ttag: TypeTag[Edge[ID]] = implicitly[TypeTag[Edge[ID]]]
  }

  implicit def lnodeMeta[ID : Ordering : Meta, L : Meta] = new Meta[LNode[ID, L]] {
    override def ctag: ClassTag[LNode[ID, L]] = implicitly[ClassTag[LNode[ID, L]]]
    override def ttag: TypeTag[LNode[ID, L]] = implicitly[TypeTag[LNode[ID, L]]]
  }

  implicit def ledgeMeta[ID : Ordering : Meta, L : Meta] = new Meta[LEdge[ID, L]] {
    override def ctag: ClassTag[LEdge[ID, L]] = implicitly[ClassTag[LEdge[ID, L]]]
    override def ttag: TypeTag[LEdge[ID, L]] = implicitly[TypeTag[LEdge[ID, L]]]
  }
  //@formatter:on
}

// -----------------------------------------------------------------------------
// Library functions
// -----------------------------------------------------------------------------

@emma.lib
object transitiveClosure {

  def apply[ID: Ordering : Meta](paths: DataBag[Edge[ID]]): DataBag[Edge[ID]] = {

    var closr = paths
    var count = closr.size
    var added = 0L

    do {
      val delta = for {
        e1 <- closr
        e2 <- closr
        if e1.tgt == e2.src
      } yield Edge(e1.src, e2.tgt)

      closr = (closr union delta).distinct

      added = closr.size - count
      count = closr.size
    } while (added > 0)

    closr
  }
}

@emma.lib
object connectedComponents {

  def apply[ID: Ordering : Meta](edges: DataBag[Edge[ID]]): DataBag[DataBag[Node[ID]]] = {

    val nodes = deriveNodes(edges)

    var components = nodes groupBy (n => n.id)

    while (true /* TODO */) {
      for {
        Group(k1, c1) <- components
        Group(k2, c2) <- components
        if k1 < k2
      } yield {
        ???
      }
    }

    components map { case Group(_, vals) => vals }
  }
}

@emma.lib
object deriveNodes {

  def apply[ID: Ordering : Meta](edges: DataBag[Edge[ID]]) =
    (edges.map(e => Node(e.src)) union edges.map(e => Node(e.tgt))).distinct

}
