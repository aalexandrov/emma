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

import api._
import compiler.BaseCompilerSpec
import compiler.RuntimeCompiler
import io.csv._
import test.TestMacros
import test.schema.Movies._
import test.util._

import com.sun.tools.corba.se.idl.StringGen

trait ComprehensionCompilerSpec extends BaseCompilerSpec {

  override lazy val compiler = new RuntimeCompiler

  import ComprehensionCompilerSpec.Example
  import TestMacros.enclose
  import TestMacros.reify

  val imdbPath = materializeResource("/cinema/imdb.csv")
  val berlinalePath = materializeResource("/cinema/berlinalewinners.csv")
  val cannesPath = materializeResource("/cinema/canneswinners.csv")

  def imdb = DataBag.readCSV[ImdbMovie]("file://" + imdbPath, CSV())
  def berlinale = DataBag.readCSV[FilmFestWinner]("file://" + berlinalePath, CSV())
  def cannes = DataBag.readCSV[FilmFestWinner]("file://" + cannesPath, CSV())

  val `Movie titles after 2000` = {
    val act = enclose()(reify {
      val m1 = ImdbMovie("Fake Title", 3.5, 1001, "http://fake-title.movie", 2001)
      val xs = Seq(m1)
      val xb = DataBag(xs)
      for {
        mv <- {
          xb
        }
        if {
          val year = mv.year
          val b1 = year > 2000
          b1
        }
      } yield {
        val title = mv.title
        title
      }
    })

    val exp = () => {
      val m1 = ImdbMovie("Fake Title", 3.5, 1001, "http://fake-title.movie", 2001)
      val xs = Seq(m1)
      val xb = DataBag(xs)
      for {
        mv <- {
          xb
        }
        if {
          val year = mv.year
          val b1 = year > 2000
          b1
        }
      } yield {
        val title = mv.title
        title
      }
    }

    Example(act, (_: Unit) => exp.apply())
  }

  val `Movies before 1991` = {
    val imdb = this.imdb

    val act = enclose(imdb)(reify {
      for {
        mv <- imdb
        if mv.year < 1991
      } yield mv
    })

    val exp = (imdb: DataBag[ImdbMovie]) =>
      for {
        mv <- imdb
        if mv.year < 1991
      } yield mv

    Example(act, exp)
  }

  val `Berlinale winners' ratings` = {
    val berlinale = this.berlinale

    val act = enclose(berlinale, this)(reify {
      for {
        mov <- imdb
        winner <- berlinale
        if mov.title == winner.title
      } yield (mov.title, mov.rating, winner.year)
    })

    val exp = (berlinale: DataBag[FilmFestWinner], this$: ComprehensionCompilerSpec) =>
      for {
        mov <- this$.imdb
        winner <- berlinale
        if mov.title == winner.title
      } yield (mov.title, mov.rating, winner.year)

    Example(act, exp.tupled)
  }

  val `Best rank out of original and remake` = {
    val act = enclose(this)(reify {
      for {
        original <- imdb
        remake <- imdb
        if original.title == remake.title
        if original.year < remake.year
      } yield (original.title, original.rank min remake.rank)
    })

    val exp = (this$: ComprehensionCompilerSpec) =>
      for {
        original <- this.imdb
        remake <- this.imdb
        if original.title == remake.title
        if original.year < remake.year
      } yield (original.title, original.rank min remake.rank)

    Example(act, exp)
  }

  val `Countries that won both festivals in the same year` = {
    val berlinale = this.berlinale
    val cannes = this.cannes

    val act = enclose(berlinale, cannes)(reify {
      for {
        bwin <- berlinale
        cwin <- cannes
        //if (bwin.year, bwin.country) == (cwin.year, cwin.country)
        if bwin.year == cwin.year
        if bwin.country == cwin.country
      } yield (bwin.year, bwin.country)
    })

    val exp = (berlinale: DataBag[FilmFestWinner], cannes: DataBag[FilmFestWinner]) =>
      for {
        bwin <- berlinale
        cwin <- cannes
        //if (bwin.year, bwin.country) == (cwin.year, cwin.country)
        if bwin.year == cwin.year
        if bwin.country == cwin.country
      } yield (bwin.year, bwin.country)

    Example(act, exp.tupled)
  }

  val `Number of festival winners per country` = {
    val ffw = (berlinale union cannes).groupBy(_.country)

    val act = enclose(ffw)(reify {
      for {
        Group(year, winners) <- ffw
      } yield (year, winners.size)
    })

    val exp = (ffw: DataBag[Group[String, DataBag[FilmFestWinner]]]) =>
      for {
        Group(year, winners) <- ffw
      } yield (year, winners.size)

    Example(act, exp)
  }

  val `Average ratings per year` = {
    val group = imdb.groupBy(_.year)

    val act = enclose(group)(reify {
      for {
        Group(year, movs) <- group
      } yield (year, movs.map(_.rating).sum / movs.size)
    })

    val exp = (group: DataBag[Group[Int, DataBag[ImdbMovie]]]) =>
      for {
        Group(year, movs) <- group
      } yield (year, movs.map(_.rating).sum / movs.size)

    Example(act, exp)
  }
}

object ComprehensionCompilerSpec {

  case class Example[C, R](act: Reified[C, R], exp: C => R)

}
