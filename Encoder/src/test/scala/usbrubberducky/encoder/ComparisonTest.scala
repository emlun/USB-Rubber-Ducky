// USB Rubber Ducky DuckyScript Encoder - Scala version
// Copyright (C) 2015 Emil Lundberg <lundberg.emil@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

package usbrubberducky
package encoder

import scala.collection.JavaConverters._
import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers

import lang.{Lexer,Parser}
import util.Context

import test._

class ComparisonTest extends FunSpec with Matchers with TestHelpers {

  describe("The new encoder") {

    describe("produces the same output as the old encoder") {

      "src/test/resources/dev-test-with-defaultdelay.ducky" ::
      "src/test/resources/dev-test-without-defaultdelay.ducky" ::
      Nil foreach { inputFile =>

        it(s"on $inputFile") {
          val oldBytes: List[Byte] = {
            val enc = new Encoder(inputFile, null, null)
            enc.setup()
            enc.encode().asScala.map(Byte.unbox(_)).toList
          }

          val newBytes: List[Byte] =
            (Lexer andThen Parser andThen NewEncoder).run(newContext)(Source fromFile inputFile)

          newBytes should be (oldBytes)
        }

      }

    }

  }
}
