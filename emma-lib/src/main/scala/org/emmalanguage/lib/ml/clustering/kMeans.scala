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
package lib.ml.clustering

import api.Meta.Projections._
import api._
import lib.linalg._
import lib.ml._
import lib.stats._
import util.RanHash

@emma.lib
object kMeans {

  /**
   * K-Means clustering algorithm.
   *
   * @param D          Number of dimensions
   * @param k          Number of clusters
   * @param runs       Number of runs.
   * @param iterations Number of iterations.
   * @param seed       Centroids seed.
   * @param points     Input points.
   * @tparam PID Point identity type
   */
  def apply[PID: Meta](
    D: Int,
    k: Int,
    runs: Int,
    iterations: Int,
    seed: Long = 452642543145L
  )(
    points: DataBag[DPoint[PID]]
  ): DataBag[Solution[PID]] = {
    // helper method: orders points `x` based on their distance to `pos`
    val distanceTo = (pos: DVector) => Ordering.by { x: DPoint[PID] =>
      sqdist(pos, x.pos)
    }

    var optSolution = DataBag.empty[Solution[PID]]
    var minSqrDist = 0.0

    for (run <- 1 to runs) {
      // initialize forgy cluster means
      var ctrds = DataBag(points.sample(k, RanHash(seed, run).seed))

      // initialize solution: label points with themselves
      var solution = for (p <- points) yield LDPoint(p.id, p.pos, p)

      for (_ <- 1 to iterations) {
        // update solution: label each point with its nearest cluster
        solution = for (s <- solution) yield {
          val closest = ctrds.min(distanceTo(s.pos))
          s.copy(label = closest)
        }
        // update centroid positions as mean of associated points
        ctrds = for {
          Group(cid, ps) <- solution.groupBy(_.label.id)
        } yield {
          val sum = stat.sum(D)(ps.map(_.pos))
          val cnt = ps.size.toDouble
          val avg = sum * (1 / cnt)
          DPoint(cid, avg)
        }
      }

      val sumSqrDist = (for (p <- solution) yield {
        sqdist(p.label.pos, p.pos)
      }).sum

      if (run <= 1 || sumSqrDist < minSqrDist) {
        minSqrDist = sumSqrDist
        optSolution = solution
      }
    }

    optSolution
  }

  type Solution[PID] = LDPoint[PID, DPoint[PID]]

}
