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

import lang.Tokens
import Tokens.IntLit
import Tokens.KeyName
import Tokens.StringLit

object Trees {

  sealed abstract class Tree(pos: Position) extends Positioned(pos)

  case class Script(defaultDelay: Option[DefaultDelay], statements: List[Statement])
    extends Tree(NoPosition)

  case class DefaultDelay(delay: IntLit) extends Tree(delay.pos)

  sealed abstract class Statement(pos: Position) extends Tree(pos)

  case class KeyPress(keyName: KeyName) extends Statement(keyName.pos)

  case class Alt(key: Option[KeyPress],           override val pos: Position) extends Statement(pos)
  case class AltShift(key: Option[KeyPress],      override val pos: Position) extends Statement(pos)
  case class AltTab(                              override val pos: Position) extends Statement(pos)
  case class Command(key: Option[KeyPress],       override val pos: Position) extends Statement(pos)
  case class CommandOption(key: Option[KeyPress], override val pos: Position) extends Statement(pos)
  case class Ctrl(key: Option[KeyPress],          override val pos: Position) extends Statement(pos)
  case class CtrlAlt(key: Option[KeyPress],       override val pos: Position) extends Statement(pos)
  case class CtrlShift(key: Option[KeyPress],     override val pos: Position) extends Statement(pos)
  case class Shift(key: Option[KeyPress],         override val pos: Position) extends Statement(pos)
  case class Super(key: Option[KeyPress],         override val pos: Position) extends Statement(pos)

  case class Delay(milliseconds: IntLit)  extends Statement(milliseconds.pos)
  case class Repeat(times: IntLit)        extends Statement(times.pos)
  case class TypeString(value: StringLit) extends Statement(value.pos)

}
