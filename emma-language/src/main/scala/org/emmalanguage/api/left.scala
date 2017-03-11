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
package api

import api.Meta.Projections._

class left[A: Meta] private(xs: Seq[A]) extends Serializable {

  def on(p: A => Boolean): DataBag[Option[A]] = {
    val rs = xs.filter(p)
    if (rs.isEmpty) DataBag(Seq(Option.empty[A]))
    else DataBag(rs.map(Some(_)))
  }

}

/**
 * Left outer join syntax.
 *
 * A left join between `xs: DataBag[X]` and `ys: DataBag[Y]` where
 * `x.a` is equal to `y.b` can be written in a for-comprehension as follows.
 *
 * {{{
 * val zs: DataBag[(X, Option[Y])] = for {
 *   x <- xs
 *   y <- left join ys on (x.a == _.b)
 * } yield (x, y)
 * }}}
 *
 * The binding `y: Option[Y]` is
 *
 * - `None`    iff the outer `x` has no matching `y` in `ys`,
 * - `Some(v)` for each `v` in `ys` that matches the outer `x`.
 */
object left {
  def join[A: Meta](xs: DataBag[A]): left[A] = new left(xs.fetch())
}
