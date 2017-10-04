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

import test._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class ForkPipelineSpec extends FunSpec with Matchers with TestHelpers {

  describe("A ForkPipeline") {

    it("can be composed with a basic Pipeline, yielding a new ForkPipeline.") {
      val divSelf = new ForkPipeline[Int, Boolean, Int] {
        override def run(ctx: Context)(i: Int) = if (i == 0) Left(false) else Right(i / i)
      }
      val addTwo  = new Pipeline[Int, Int] { override def  run(ctx: Context)(i: Int) = i + 2 }
      val composition: ForkPipeline[Int, Boolean, Int] = divSelf andThen addTwo

      composition.run(newContext)(1).right.get should be (3)
      composition.run(newContext)(0).left.get should be (false)
    }

    it("can be composed with another ForkPipeline, yielding a new ForkPipeline.") {
      val divThree = new ForkPipeline[Int, Boolean, Int] {
        override def run(ctx: Context)(i: Int) = if (i == 0) Left(false) else Right(i / 3)
      }
      val addTwoUnlessOne = new ForkPipeline[Int, Boolean, Int] {
        override def run(ctx: Context)(i: Int) = if (i == 1) Left(true) else Right(i + 2)
      }
      val composition: ForkPipeline[Int, Boolean, Int] = divThree andThen addTwoUnlessOne

      composition.run(newContext)(1).right.get should be (2)
      composition.run(newContext)(3).left.get should be (true)
    }

    it("is evaluated if and only if all previous ForkPipelines in the composition succeeded.") {
      val abortOdd = new ForkPipeline[Int, Boolean, Int] {
        override def run(ctx: Context)(i: Int) = if (i % 2 == 0) Right(i) else Left(true)
      }
      val addTwo        = new Pipeline[Int, Int]     { override def run(ctx: Context)(i: Int) = i + 2 }
      val failIfReached = new Pipeline[Any, Nothing] { override def run(ctx: Context)(a: Any) =
        fail("This code should not be reached.")
      }

      val doAdd: ForkPipeline[Int, Boolean, Int] = abortOdd andThen addTwo
      doAdd.run(newContext)(2).right.get should be (4)

      val failFast = abortOdd andThen failIfReached
      failFast.run(newContext)(1).left.get should be (true)
    }

  }

}
