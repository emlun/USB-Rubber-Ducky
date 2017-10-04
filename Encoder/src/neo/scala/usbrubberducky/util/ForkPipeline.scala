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

import scala.util.Either

trait ForkPipeline[-I, L, +R] extends Pipeline[I, Either[L, R]] {

  def andThen[NextO](next: Pipeline[R, NextO]): ForkPipeline[I, L, NextO] = {
    val first = this;
    new ForkPipeline[I, L, NextO]() {
      override def run(ctx: Context)(input: I): Either[L, NextO] =
        first.run(ctx)(input).right.map(next.run(ctx))
    }
  }

  def andThen[NextR](next: ForkPipeline[R, L, NextR]): ForkPipeline[I, L, NextR] = {
    val first = this;
    new ForkPipeline[I, L, NextR]() {
      override def run(ctx: Context)(input: I): Either[L, NextR] =
        first.run(ctx)(input).right.flatMap(next.run(ctx))
    }
  }

}
