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
package compiler

import spark.SparkBackend
import spark.SparkSpecializeSupport

trait SparkCompiler extends Compiler
  with SparkBackend
  with SparkSpecializeSupport {

  override lazy val implicitTypes: Set[u.Type] = API.implicitTypes ++ SparkAPI.implicitTypes

  trait NtvAPI extends ModuleAPI {
    //@formatter:off
    val Broadcast         = api.Type[org.emmalanguage.api.spark.SparkNtv.BroadcastBag[Any]].typeConstructor
    val sym               = api.Sym[org.emmalanguage.api.spark.SparkNtv.type].asModule

    val select            = op("select")
    val project           = op("project")
    val equiJoin          = op("equiJoin")

    val broadcast         = op("broadcast")
    val bag               = op("bag")

    override lazy val ops = Set(select, project, equiJoin, broadcast, bag)
    //@formatter:on
  }

  trait SparkExpAPI extends ModuleAPI {
    //@formatter:off
    val Column      = api.Type[org.apache.spark.sql.Column]
    val Type        = api.Type[org.emmalanguage.api.spark.SparkExp.Expr]
    val Root        = api.Type[org.emmalanguage.api.spark.SparkExp.Root]
    val sym         = api.Sym[org.emmalanguage.api.spark.SparkExp.type].asModule

    // projections
    val proj        = op("proj")
    val struct      = op("struct")
    // comparisons
    val eq          = op("eq", List(2, 1))
    val ne          = op("ne", List(2, 1))
    val gt          = op("gt")
    val lt          = op("lt")
    val geq         = op("geq")
    val leq         = op("leq")
    // boolean
    val not         = op("not")
    val or          = op("or")
    val and         = op("and")
    // arithmetic
    val plus        = op("plus")
    val minus       = op("minus")
    val multiply    = op("multiply")
    val divide      = op("divide")
    val mod         = op("mod")
    // string
    val startsWith  = op("startsWith")

    val structural  = Set(proj, struct)
    val comparisons = Set(eq, ne, lt, gt, leq, geq)
    val boolean     = Set(not, and, or)
    val arithmetic  = Set(plus, minus, multiply, divide, mod)
    val string      = Set(startsWith)

    val ops         = structural ++ comparisons ++ boolean ++ arithmetic ++ string
    //@formatter:on
  }

  object SparkAPI extends BackendAPI {
    lazy val Encoder = api.Type[org.apache.spark.sql.Encoder[Any]].typeConstructor
    lazy val SparkSession = api.Type[org.apache.spark.sql.SparkSession]

    lazy val implicitTypes = Set(Encoder, SparkSession)

    lazy val DataBag = new DataBagAPI(api.Sym[org.emmalanguage.api.SparkDataset[Any]].asClass)

    lazy val DataBag$ = new DataBag$API(api.Sym[org.emmalanguage.api.SparkDataset.type].asModule)

    lazy val MutableBag = new MutableBagAPI(api.Sym[org.emmalanguage.api.SparkMutableBag[Any, Any]].asClass)

    lazy val MutableBag$ = new MutableBag$API(api.Sym[org.emmalanguage.api.SparkMutableBag.type].asModule)

    lazy val Ops = new OpsAPI(api.Sym[org.emmalanguage.api.spark.SparkOps.type].asModule)

    lazy val Ntv = new NtvAPI {}

    lazy val Exp = new SparkExpAPI {}
  }

}
