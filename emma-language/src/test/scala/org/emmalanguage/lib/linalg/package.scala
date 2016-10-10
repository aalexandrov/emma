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
package lib.linalg

import org.emmalanguage.api._

// -----------------------------------------------------------------------------
// Library functions
// -----------------------------------------------------------------------------

@emma.lib
object plus {
  def apply[A: Numeric](x: A, y: A): A = {
    val n = implicitly[Numeric[A]]
    n.plus(x, y)
  }
}

@emma.lib
object times {
  def apply[A: Numeric](x: A, y: A): A = {
    val n = implicitly[Numeric[A]]
    n.times(x, y)
  }
}

@emma.lib
object square {
  def apply[A: Numeric](x: A): A = {
    times(x, x)
  }
}