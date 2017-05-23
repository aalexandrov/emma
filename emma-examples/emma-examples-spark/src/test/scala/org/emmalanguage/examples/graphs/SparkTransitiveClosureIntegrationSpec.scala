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

import api._
import api.spark._
import model.Edge

import org.apache.spark.sql.SparkSession

import scala.util.control.TailCalls._

class SparkTransitiveClosureIntegrationSpec extends BaseTransitiveClosureIntegrationSpec with SparkAware {

  override def transitiveClosure(input: String, csv: CSV): Set[Edge[Long]] =
    withDefaultSparkSession(implicit spark => emma.onSpark("/dataset.conf") {
      // read in set of edges
      val edges = DataBag.readCSV[Edge[Long]](input, csv)
      // build the transitive closure
      val paths = TransitiveClosure(edges)
      // return the closure as local set
      paths.collect().toSet
    }/*rslt(input, csv)*/)

  def rslt(input: String, csv: CSV)(implicit spark: SparkSession) = {
    val edges = SparkDataset.readCSV[Edge[Long]](input, csv)
    val distinct$m1 = edges.distinct
    val distinct$m1$m1 = SparkOps cache distinct$m1
    val size$m1 = distinct$m1$m1.size
    def doWhile$m1(added$m1$m1: Long, count$m1$m1: Long, paths$m1$m1: DataBag[Edge[Long]]): TailRec[Set[Edge[Long]]] = {
      val delta$m1 = { // FIXME: why is this nested?
        val kx$m1 = (e1$m1: String) => {
          val dst$m1 = SparkExp.rootProj(e1$m1, "dst")
          val r$m1 = SparkExp.rootStruct("_1")(dst$m1)
          r$m1
        }
        val ky$m1 = (e2$m2: String) => {
          val src$m1 = SparkExp.rootProj(e2$m2, "src")
          val r$m2 = SparkExp.rootStruct("_1")(src$m1)
          r$m2
        }
        val joined$m1 = SparkNtv.equiJoin[Edge[Long], Edge[Long], Long](kx$m1, ky$m1)(paths$m1$m1, paths$m1$m1)
        val f$m1 = (xy$m1: String) => {
          val e1$m1 = SparkExp.rootProj(xy$m1, "_1")
          val e2$m2 = SparkExp.rootProj(xy$m1, "_2")
          val src$m2 = SparkExp.nestProj(e1$m1, "src")
          val dst$m2 = SparkExp.nestProj(e2$m2, "dst")
          val apply$m1 = SparkExp.rootStruct("src", "dst")(src$m2, dst$m2)
          apply$m1
        }
        val mapped$m1 = SparkNtv.project[(Edge[Long], Edge[Long]), Edge[Long]](f$m1)(joined$m1)
        mapped$m1
      }
      val union$m1 = paths$m1$m1 union delta$m1
      val distinct$m2 = union$m1.distinct
      val distinct$m2$m1 = SparkOps cache distinct$m2
      val size$m2 = distinct$m2$m1.size
      val `-$m1` = size$m2 - count$m1$m1
      val `>$m1` = `-$m1` > 0
      def suffix$m1(): TailRec[Set[Edge[Long]]] = {
        val collect$m1 = distinct$m2$m1.collect()
        val toSet$m1 = collect$m1.toSet[Edge[Long]]
        done(toSet$m1)
      }
      if (`>$m1`) tailcall(doWhile$m1(`-$m1`, size$m2, distinct$m2$m1)) else tailcall(suffix$m1())
    }
    doWhile$m1(0L, size$m1, distinct$m1$m1).result
  }
}
