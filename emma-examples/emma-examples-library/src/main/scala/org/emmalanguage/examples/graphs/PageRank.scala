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
package examples.graphs

import api.Meta.Projections._
import api._
import model._

@emma.lib
object PageRank {

  def apply[V: Ordering : Meta.Tag](
    d: Double, iterations: Int // hyper-parameters
  )(
    edges: DataBag[Edge[V]]
  ): DataBag[LVertex[V, Double]] = {
    // outdegrees for vertices with at least one outbound edge
    val odegs = for {
      Group(vid, vs) <- edges.groupBy(_.src)
    } yield LVertex(vid, vs.map(_.dst).size)

    // initial ranks
    var ranks = for {
      v <- (edges.map(_.src) union edges.map(_.dst)).distinct
    } yield LVertex(v, 1.0)

    for (i <- 1 to iterations) {
      // distributed current rank evenly among all outgoing edges
      val msgs = for {
        r <- ranks
        d <- odegs
        e <- edges
        if r.id == d.id
        if r.id == e.src
      } yield Message(e.dst, r.label / d.label)

      // sum incoming ranks
      val sums = for {
        Group(tgt, msgs) <- msgs.groupBy(_.tgt)
      } yield Message(tgt, msgs.map(_.payload).sum)

      // compute new ranks
      ranks = for {
        r <- ranks
        s <- left join sums on (r.id == _.tgt)
      } yield LVertex(r.id, (1.0 - d) + d * (s.map(_.payload) getOrElse 0.0))
    }

    ranks
  }
}
