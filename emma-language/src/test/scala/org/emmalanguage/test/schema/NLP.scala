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
package test.schema

import api._

object NLP {

  //@formatter:off
  case class Position
  (
    docID  : Long,
    offset : Int,
    length : Int
  )

  case class Doc
  (
    id   : Long,
    text : String,
    lang : String
  )
  //@formatter:on

  case class Sentence(pos: Position, text: String)

  case class NamedEntity(pos: Position, text: String)

  val docs = DataBag.empty[Doc]

  def doc2sentences(d: String): DataBag[Sentence] =
    DataBag.empty

  def maleNames(lang: String)(sentence: String): DataBag[Sentence] =
    DataBag.empty

  def companyNames(sentence: String): DataBag[Sentence] =
    DataBag.empty
}
