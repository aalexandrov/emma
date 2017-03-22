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
package compiler.lang.opt

class JoinOrderOptimizerSpec extends ComprehensionCompilerSpec {

  import compiler.ComprehensionCompiler.{apply => comprehend}

  "Movie titles after 2000" in {
    val exp = `Movie titles after 2000`.exp(Unit)
    val act = comprehend(`Movie titles after 2000`.act)(Unit)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Movies before 1991" in {
    val exp = `Movies before 1991`.exp(imdb)
    val act = comprehend(`Movies before 1991`.act)(imdb)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Berlinale winners' ratings" in {
    val berlinale = this.berlinale

    val act = comprehend(`Berlinale winners' ratings`.act)(berlinale, this)
    val exp = `Berlinale winners' ratings`.exp(berlinale, this)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Best rank out of original and remake" in {
    val imdb = this.imdb

    val act = comprehend(`Best rank out of original and remake`.act)(this)
    val exp = `Best rank out of original and remake`.exp(this)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Countries that won both festivals in the same year" in {
    val berlinale = this.berlinale
    val cannes = this.cannes

    val act = comprehend(`Countries that won both festivals in the same year`.act)(berlinale, cannes)
    val exp = `Countries that won both festivals in the same year`.exp(berlinale, cannes)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Number of festival winners per country" in {
    val ffw = (berlinale union cannes).groupBy(_.country)

    val act = comprehend(`Number of festival winners per country`.act)(ffw)
    val exp = `Number of festival winners per country`.exp(ffw)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }

  "Average ratings per year" in {
    val group = imdb.groupBy(_.year)

    val act = comprehend(`Average ratings per year`.act)(group)
    val exp = `Average ratings per year`.exp(group)

    act.fetch() should contain theSameElementsAs exp.fetch()
  }
}
