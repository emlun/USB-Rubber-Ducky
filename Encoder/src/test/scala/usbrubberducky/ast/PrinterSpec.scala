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
package ast

import java.io.File

import scala.io.Source
import scala.util.Try

import org.scalatest.FunSpec
import org.scalatest.Matchers

import lang.Lexer
import lang.Parser

import util.Context
import util.Pipeline

class PrinterSpec extends FunSpec with Matchers {

  private object StringToSource extends Pipeline[String, Source] {
    override def run(ctx: Context)(s: String) = Source fromString s
  }

  describe("The pretty printer") {

    it("outputs the same thing again if run on its own output.") {
      val path = "/oneofeach-space-padded.ducky"
      val input = Try(new File(getClass.getResource(path).toURI())) getOrElse fail("File " + path + " not found.")

      var firstOutput = ""

      val pipeline = Lexer andThen Parser andThen PrettyPrinter andThen new Pipeline[String, String]() {
          override def run(ctx: Context)(s: String) = {
            firstOutput = s
            s
          }
        } andThen StringToSource andThen Lexer andThen Parser andThen PrettyPrinter

      val secondOutput = pipeline.run(new Context())(Source fromFile input)

      secondOutput should be (firstOutput)
    }

  }

}
