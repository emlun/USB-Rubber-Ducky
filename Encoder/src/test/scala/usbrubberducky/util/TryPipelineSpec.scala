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
import org.scalatest.TryValues

class TryPipelineSpec extends FunSpec with Matchers with TryValues with TestHelpers {

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

    it("is evaluated if and only if all previous TryPipelines in the composition succeeded.") {
      val divSelf       = new TryPipeline[Int, Int]  { override def tryRun(ctx: Context)(i: Int) = i / i }
      val addTwo        = new Pipeline[Int, Int]     { override def    run(ctx: Context)(i: Int) = i + 2 }
      val failIfReached = new Pipeline[Any, Nothing] { override def    run(ctx: Context)(a: Any) =
        fail("This code should not be reached.")
      }

      val divAdd: TryPipeline[Int, Int] = divSelf andThen addTwo
      divAdd.run(newContext)(1).success.value should be (3)

      val failFast = divSelf andThen failIfReached
      failFast.run(newContext)(1) should be a 'failure
    }

  }

}
