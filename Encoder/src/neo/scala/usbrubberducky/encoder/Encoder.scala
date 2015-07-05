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

import ast.Trees._
import lang.Tokens._
import util.Context
import util.Pipeline


object NewEncoder extends Pipeline[Script, List[Byte]] {

  private def encodeDelay(milliseconds: Int): List[Byte] =
    List.fill(milliseconds / 255)(List(0x00.toByte, 0xFF.toByte)).flatten ++:
    (milliseconds % 255 match {
      case 0         => Nil
      case remainder => List(0x00.toByte, remainder.toByte)
    }) ++:
    Nil

  private def encodeStatement(ctx: Context, defaultDelay: Option[DefaultDelay])
                             (statement: Statement): List[Byte] = {
    val defaultDelayBytes = defaultDelay map { delay => encodeDelay(delay.milliseconds.value) } getOrElse Nil

    statement match {
      case Delay(IntLit(milliseconds, _)) => encodeDelay(milliseconds)
    }
  }

  override def run(ctx: Context)(script: Script) = {

    val bytes: List[Byte] = script.statements flatMap encodeStatement(ctx, script.defaultDelay) _

    bytes
  }

}
