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
import util.PositionedBy
import util.NoPosition

import lang.Tokens
import Tokens.IntLit
import Tokens.KeyName
import Tokens.StringLit

object Trees {

  sealed trait Tree extends Positioned

  case class Script(defaultDelay: Option[DefaultDelay], statements: List[Statement]) extends Tree {
    override def pos = NoPosition
  }

  case class DefaultDelay(milliseconds: IntLit) extends PositionedBy(milliseconds) with Tree
  case class Repeat(times: IntLit)              extends PositionedBy(times)        with Tree

  sealed trait Statement extends Tree {
    def repeat: Option[Repeat]
  }

  case class KeyPress(keyName: KeyName, repeat: Option[Repeat]) extends PositionedBy(keyName) with Statement

  case class Alt(key: Option[KeyName],           repeat: Option[Repeat], pos: Position) extends Statement
  case class AltShift(key: Option[KeyName],      repeat: Option[Repeat], pos: Position) extends Statement
  case class AltTab(                             repeat: Option[Repeat], pos: Position) extends Statement
  case class Command(key: Option[KeyName],       repeat: Option[Repeat], pos: Position) extends Statement
  case class CommandOption(key: Option[KeyName], repeat: Option[Repeat], pos: Position) extends Statement
  case class Ctrl(key: Option[KeyName],          repeat: Option[Repeat], pos: Position) extends Statement
  case class CtrlAlt(key: Option[KeyName],       repeat: Option[Repeat], pos: Position) extends Statement
  case class CtrlShift(key: Option[KeyName],     repeat: Option[Repeat], pos: Position) extends Statement
  case class Shift(key: Option[KeyName],         repeat: Option[Repeat], pos: Position) extends Statement
  case class Super(key: Option[KeyName],         repeat: Option[Repeat], pos: Position) extends Statement

  case class Delay(milliseconds: IntLit, repeat: Option[Repeat])  extends PositionedBy(milliseconds) with Statement
  case class TypeString(value: StringLit, repeat: Option[Repeat]) extends PositionedBy(value)        with Statement

}
