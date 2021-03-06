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

trait TryPipeline[-I, +O] extends Pipeline[I, Try[O]] {
  override def run(ctx: Context)(input: I) = Try(tryRun(ctx)(input))
  def tryRun(ctx: Context)(input: I): O

  def andThen[NextO](next: Pipeline[O, NextO]): TryPipeline[I, NextO] =
    andThen(new TryPipeline[O, NextO]() {
      override def tryRun(ctx: Context)(input: O) = next.run(ctx)(input)
    })

  def andThen[NextO](next: TryPipeline[O, NextO]): TryPipeline[I, NextO] = {
    val first = this;
    new TryPipeline[I, NextO]() {
      override def run(ctx: Context)(input: I): Try[NextO] = first.run(ctx)(input).flatMap(next.run(ctx))
      override def tryRun(ctx: Context)(input: I): Nothing =
        throw new UnsupportedOperationException("This code should be unreachable - please file a bug report.")
    }
  }
}
