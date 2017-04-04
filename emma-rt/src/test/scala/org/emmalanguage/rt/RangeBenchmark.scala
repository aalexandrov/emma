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

import org.scalameter.KeyValue
import org.scalameter.api._

object RangeBenchmark extends Bench.ForkedTime {

  /* inputs */

  val sizes = Gen.range("size")(300000, 1500000, 300000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  /* tests */

  performance of "Range" config(

  ) in {
    measure method "sum (foreach)" in {
      using(ranges) in { r =>
        var s = 0
        r.foreach(i => s += i)
      }
    }

    measure method "sum (while)" in {
      using(ranges) in { r =>
        var i = r.min
        val M = r.max
        var s = 0
        while (i <= M) {
          s += i
          i += 1
        }
      }
    }

    measure method "sum (do-while)" in {
      using(ranges) in { r =>
        var i = r.min
        val M = r.max
        var s = 0
        do {
          s += i
          i += 1
        } while (i <= M)
      }
    }
  }
}
