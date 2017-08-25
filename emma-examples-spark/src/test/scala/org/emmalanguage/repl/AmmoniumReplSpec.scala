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
package repl

import api._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class AmmoniumReplSpec extends FlatSpec with Matchers {

  import AmmoniumReplSpec._

  "class-based Ammonium REPL" should "compile and evaluate properly" in {
    cmd1.wrapper.wrapper.act should contain theSameElementsAs cmd0.wrapper.wrapper.exp
  }
}


object AmmoniumReplSpec extends SparkAware {

  object cmd0 {
    val wrapper = new cmd0Wrapper
  }

  final class cmd0Wrapper extends java.io.Serializable {
    val wrapper = new Helper

    final class Helper extends java.io.Serializable {
      val exp = Seq(17)
    }

  }

  object cmd1 {
    lazy val cmd0: AmmoniumReplSpec.cmd0.wrapper.type = AmmoniumReplSpec.cmd0.wrapper

    val wrapper = new cmd1Wrapper
  }

  final class cmd1Wrapper extends java.io.Serializable {
    lazy val cmd0: AmmoniumReplSpec.cmd0.wrapper.type = AmmoniumReplSpec.cmd0.wrapper

    import cmd0.wrapper.exp

    val wrapper = new Helper

    final class Helper extends java.io.Serializable {
      val act = withDefaultSparkSession(implicit spark => emma.onSpark {
        DataBag(exp).collect()
      })
    }

  }

}

