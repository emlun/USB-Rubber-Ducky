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
package util

import scala.util.Try

import test._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.TryValues

class PipelineSpec extends FunSpec with Matchers with TryValues with TestHelpers {

  private object Doubler extends Pipeline[Int, Int] {
    override def run(ctx: Context)(i: Int): Int = i * 2
  }

  private class A
  private class B extends A

  describe("A Pipeline") {

    it("has an input and output type.") {
      val pipeline = new Pipeline[String, Int] {
        override def run(ctx: Context)(s: String) = s.length
      }
      val input: String = "foo"
      val output: Int = pipeline.run(newContext)(input)
      output should be (3)
    }

    it("can transform the input.") {
      val func = Doubler.run(newContext) _
      func(1) should be (2)
      func(2) should be (4)
      func(3) should be (6)
    }

    it("is contravariant in the input type.") {
      val general: Pipeline[A, Int] = new Pipeline[A, Int] { override def run(ctx: Context)(a: A) = 0 }
      val specific: Pipeline[B, Int] = general
      general should be theSameInstanceAs specific
    }

    it("is covariant in the output type.") {
      val specific: Pipeline[Int, B] = new Pipeline[Int, B] { override def run(ctx: Context)(i: Int) = new B }
      val general: Pipeline[Int, A] = specific
      general should be theSameInstanceAs specific
    }

    it("can be composed with another Pipeline.") {
      (Doubler andThen Doubler andThen Doubler andThen Doubler).run(newContext)(1) should be (16)
    }

    it("can be composed with another Pipeline with different I/O types.") {
      class A(length: Int) { val value = "a" * length }
      class B(length: Int) extends A(length)

      val makeA = new Pipeline[Int, B] { override def run(ctx: Context)(i: Int) = new B(i) }
      val unpackA = new Pipeline[A, String] { override def run(ctx: Context)(a: A) = a.value }
      val getStringLength = new Pipeline[String, Int] { override def run(ctx: Context)(s: String) = s.length }
      val isGreaterThanFive = new Pipeline[Int, Boolean] { override def run(ctx: Context)(i: Int) = i > 5 }

      (makeA andThen unpackA andThen getStringLength andThen isGreaterThanFive)
        .run(newContext)(3) should be (false)
    }

    it("passes the same context instance through the whole composition chain.") {
      val context = newContext

      class ContextChecker(val expectedContext: Context) extends Pipeline[Int, Int] {
        override def run(ctx: Context)(i: Int) = {
          ctx should be theSameInstanceAs expectedContext
          i
        }
      }

      val checker = new ContextChecker(context)
      (checker andThen checker andThen checker andThen checker andThen checker).run(context)(0)
    }

    it("executes composed pipelines in the right order.") {
      val addTwo = new Pipeline[Int, Int] { override def run(ctx: Context)(i: Int) = i + 2 }
      val mulTen = new Pipeline[Int, Int] { override def run(ctx: Context)(i: Int) = i * 10 }
      (addTwo andThen mulTen).run(newContext)(1) should be (30)
    }

    it("can be composed with a TryPipeline, yielding a Pipeline to a Try type.") {
      val addTwo  = new Pipeline[Int, Int]    { override def    run(ctx: Context)(i: Int) = i + 2 }
      val divSelf = new TryPipeline[Int, Int] { override def tryRun(ctx: Context)(i: Int) = i / i }
      val composition: Pipeline[Int, Try[Int]] = addTwo andThen divSelf

      composition.run(newContext)(1).success.value should be (1)
      composition.run(newContext)(-2) should be a 'failure
    }

  }

  describe("A TryPipeline") {

    it("can be composed with a basic Pipeline, yielding a new TryPipeline.") {
      val divSelf = new TryPipeline[Int, Int] { override def tryRun(ctx: Context)(i: Int) = i / i }
      val addTwo  = new Pipeline[Int, Int]    { override def    run(ctx: Context)(i: Int) = i + 2 }
      val composition: TryPipeline[Int, Int] = divSelf andThen addTwo

      composition.run(newContext)(1).success.value should be (3)
      composition.run(newContext)(0) should be a 'failure
    }

    it("can be composed with another TryPipeline, yielding a new TryPipeline.") {
      val divSelf = new TryPipeline[Int, Int] { override def tryRun(ctx: Context)(i: Int) = i / i }
      val addTwo  = new TryPipeline[Int, Int] { override def tryRun(ctx: Context)(i: Int) = i + 2 }
      val composition: TryPipeline[Int, Int] = divSelf andThen addTwo

      composition.run(newContext)(1).success.value should be (3)
      composition.run(newContext)(0) should be a 'failure
    }

  }

}
