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

import examples.graphs.model._
import test.util._

import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/** Examples are taken from [[http://www.sirgroane.net/google-page-rank/ this blogpost by Ian Rogers]]. */
trait BasePageRankIntegrationSpec extends FlatSpec with Matchers with BeforeAndAfter {

  val codegenDir = tempPath("codegen")

  before {
    new File(codegenDir).mkdirs()
    addToClasspath(new File(codegenDir))
  }

  after {
    deleteRecursive(new File(codegenDir))
  }

  // hyper-parameters
  val dampingFactor = 0.85
  val iterations = 8
  // expeced value tolerance
  val epsilon = 0.1

  "Example 1" should "produce correct PageRank values" in {
    val act = pageRankChar(Seq(
      'A' -> ('B' :: 'C' :: Nil),
      'B' -> ('C' :: Nil),
      'C' -> ('A' :: Nil),
      'D' -> ('C' :: Nil)
    ).flatMap { case (src, tgts) => tgts.map(Edge(src, _)) })

    val exp = Seq(
      LVertex('A', 1.49),
      LVertex('B', 0.78),
      LVertex('C', 1.58),
      LVertex('D', 0.15)
    )

    // compare vertex IDs
    act.map(_.id) should contain theSameElementsAs exp.map(_.id)
    // compare vertex labels
    for (a <- act; e <- exp if a.id == e.id)
      a.label shouldBe (e.label +- epsilon)
  }

  "Example 2" should "produce correct PageRank values" in {
    val act = pageRankString(Seq(
      //@formatter:off
      "Home"    -> ("About" :: "Product" :: "Links" :: Nil),
      "About"   -> ("Home" :: Nil),
      "Product" -> ("Home" :: Nil),
      "Links"   -> ("Home" :: ('A' to 'D').map(x => s"External Site $x").toList)
      //@formatter:on
    ).flatMap { case (src, tgts) => tgts.map(Edge(src, _)) })

    val exp = Seq(
      //@formatter:off
      LVertex("Home"           , 0.92),
      LVertex("About"          , 0.41),
      LVertex("Product"        , 0.41),
      LVertex("Links"          , 0.41),
      LVertex("External Site A", 0.22),
      LVertex("External Site B", 0.22),
      LVertex("External Site C", 0.22),
      LVertex("External Site D", 0.22)
      //@formatter:on
    )

    // compare vertex IDs
    act.map(_.id) should contain theSameElementsAs exp.map(_.id)
    // compare vertex labels
    for (a <- act; e <- exp if a.id == e.id)
      a.label shouldBe (e.label +- epsilon)
  }

  "Example 3" should "produce correct PageRank values" in {
    val act = pageRankString(Seq(
      //@formatter:off
      "Home"            -> ("About" :: "Product" :: "Links" :: Nil),
      "About"           -> ("Home" :: Nil),
      "Product"         -> ("Home" :: Nil),
      "Links"           -> ("Home" :: ('A' to 'D').map(x => s"External Site $x").toList),
      "External Site A" -> ("Home" :: Nil),
      "External Site B" -> ("Home" :: Nil),
      "External Site C" -> ("Home" :: Nil),
      "External Site D" -> ("Home" :: Nil)
      //@formatter:on
    ).flatMap { case (src, tgts) => tgts.map(Edge(src, _)) })

    val exp = Seq(
      //@formatter:off
      LVertex("Home"           , 3.35),
      LVertex("About"          , 1.10),
      LVertex("Product"        , 1.10),
      LVertex("Links"          , 1.10),
      LVertex("External Site A", 0.34),
      LVertex("External Site B", 0.34),
      LVertex("External Site C", 0.34),
      LVertex("External Site D", 0.34)
      //@formatter:on
    )

    // compare vertex IDs
    act.map(_.id) should contain theSameElementsAs exp.map(_.id)
    // compare vertex labels
    for (a <- act; e <- exp if a.id == e.id)
      a.label shouldBe (e.label +- epsilon)
  }

  "Example 4" should "produce correct PageRank values" in {
    val act = pageRankString(Seq(
      //@formatter:off
      "Home"            -> ("About" :: "Product" :: "Links" :: Nil),
      "About"           -> ("Home" :: Nil),
      "Product"         -> ("Home" :: Nil),
      "Links"           -> ("Home" :: ('A' to 'D').flatMap(x => Seq(s"External Site $x", s"Review $x")).toList),
      "Review A"        -> ("Home" :: Nil),
      "Review B"        -> ("Home" :: Nil),
      "Review C"        -> ("Home" :: Nil),
      "Review D"        -> ("Home" :: Nil)
      //@formatter:on
    ).flatMap { case (src, tgts) => tgts.map(Edge(src, _)) })

    val exp = Seq(
      //@formatter:off
      LVertex("Home"           , 2.44),
      LVertex("About"          , 0.84),
      LVertex("Product"        , 0.84),
      LVertex("Links"          , 0.84),
      LVertex("External Site A", 0.23),
      LVertex("External Site B", 0.23),
      LVertex("External Site C", 0.23),
      LVertex("External Site D", 0.23),
      LVertex("Review A"       , 0.23),
      LVertex("Review B"       , 0.23),
      LVertex("Review C"       , 0.23),
      LVertex("Review D"       , 0.23)
      //@formatter:on
    )

    // compare vertex IDs
    act.map(_.id) should contain theSameElementsAs exp.map(_.id)
    // compare vertex labels
    for (a <- act; e <- exp if a.id == e.id)
      a.label shouldBe (e.label +- epsilon)
  }

  def pageRankChar(edges: Seq[Edge[Char]]): Seq[LVertex[Char, Double]]

  def pageRankString(edges: Seq[Edge[String]]): Seq[LVertex[String, Double]]
}
