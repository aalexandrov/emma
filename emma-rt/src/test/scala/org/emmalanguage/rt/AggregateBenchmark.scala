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
package rt

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.util.Random

object AggregateBenchmark extends Bench.LocalTime {

  /* inputs */

  val N = 1000000000 // number of elements in the input sequence
  val bufferSizes = Gen.single("bufferSize")(32 * 1000)

  val bufferSeqs = for {
    bufferSize <- bufferSizes
    buffersCnt = Math.ceil(N / bufferSize.toDouble).toInt
    random = new Random(452345364423140L)
  } yield for (i <- 0 until buffersCnt) yield {
    val buffer = Array.ofDim[Int](bufferSize)
    var i = 0
    while (i < bufferSize) {
      buffer(i) = random.nextInt()
      i += 1
    }
    buffer
  }

  /* tests */

  performance of s"Summing $N integers" in {
    /*measure method "sum (outer - foreach, inner - foreach)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        bufferSeq.foreach(buffer => {
          buffer.foreach(s += _)
        })
      }
    }

    measure method "sum (outer - foreach, inner - fold)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        bufferSeq.foreach(buffer => {
          s += buffer.sum
        })
      }
    }

    measure method "sum (outer - foreach, inner - while)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        bufferSeq.foreach(buffer => {
          val bufferSize = buffer.length
          var i = 0
          while (i < bufferSize) {
            s += buffer(i)
            i += 1
          }
        })
      }
    }

    measure method "sum (outer - while, inner - foreach)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        val buffersCnt = bufferSeq.size
        var j = 0
        while (j < buffersCnt) {
          val buffer = bufferSeq(j)
          buffer.foreach(s += _)
          j += 1
        }
      }
    }

    measure method "sum (outer - while, inner - fold)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        val buffersCnt = bufferSeq.size
        var j = 0
        while (j < buffersCnt) {
          val buffer = bufferSeq(j)
          s += buffer.sum
          j += 1
        }
      }
    }*/

    measure method "sum (outer - while, inner - while)" in {
      using(bufferSeqs) in { bufferSeq =>
        var s = 0
        val buffersCnt = bufferSeq.size
        var j = 0
        while (j < buffersCnt) {
          val buffer = bufferSeq(j)
          val bufferSize = buffer.length
          var i = 0
          while (i < bufferSize) {
            s += buffer(i)
            i += 1
          }
          j += 1
        }
      }
    }
  }
}
