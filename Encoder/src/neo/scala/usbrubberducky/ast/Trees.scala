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

import util.Position
import util.Positioned
import util.NoPosition

object Trees {

  sealed class Tree(pos: Position) extends Positioned(pos)

  sealed case class Script(defaultDelay: Option[DefaultDelay], statements: List[Statement])
    extends Tree(NoPosition)

  sealed case class DefaultDelay(delay: Int, override val pos: Position) extends Tree(pos)

  sealed trait Statement extends Tree

}
