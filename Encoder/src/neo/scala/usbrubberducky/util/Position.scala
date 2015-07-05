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

import scala.util.parsing.input.OffsetPosition

/**
 * @param line 1-indexed line number
 * @param column 1-indexed column number
 * @param lineContent the content of the line this position is in
 */
case class Position(line: Int, column: Int, lineContent: String, fileName: Option[String] = None) {
  private lazy val _pos = OffsetPosition(lineContent, column - 1)

  def longString = _pos.longString
  override def toString = (fileName map { _ + ":" } getOrElse "") + line + ":" + column
}

object NoPosition extends Position(0, 0, "") {
  override def toString   = "<position unavailable>"
  override def longString = toString
}

trait Positioned {
  def pos: Position
}
abstract class PositionedBy(posProvider: Positioned) extends Positioned {
  override def pos = posProvider.pos
}
