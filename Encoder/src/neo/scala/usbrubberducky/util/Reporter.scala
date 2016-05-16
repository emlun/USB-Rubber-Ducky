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

class Reporter(
    val debug: Boolean = false,
    val info: Boolean = true,
    val warn: Boolean = true,
    val error: Boolean = true,
    val println: (Any => Unit) = Console.err.println
) {

  private var _hasErrors = false
  def hasErrors = _hasErrors

  private def report(prefix: String, message: Any, pos: Option[Position]): Unit =
    pos map { pos =>
      println(prefix + pos + ": " + message)
      println(pos.longString)
    } orElse {
      println(prefix + message)
      None
    }

  def debug(message: Any, pos: Option[Position]): Unit = if (debug) { report("DEBUG: ", message, pos) }
  def info(message: Any, pos: Option[Position]): Unit  = if (info)  { report("INFO: ", message, pos) }
  def warn(message: Any, pos: Option[Position]): Unit  = if (warn)  { report("WARN: ", message, pos) }
  def error(message: Any, pos: Option[Position]): Unit = {
    _hasErrors = true
    if (error) {
      report("ERROR: ", message, pos)
    }
  }

  def debug(message: Any): Unit = debug(message, None)
  def info(message: Any): Unit  = info(message, None)
  def warn(message: Any): Unit  = warn(message, None)
  def error(message: Any): Unit = error(message, None)

  def debug(message: Any, pos: Position): Unit = debug(message, Some(pos))
  def info(message: Any, pos: Position): Unit  = info(message, Some(pos))
  def warn(message: Any, pos: Position): Unit  = warn(message, Some(pos))
  def error(message: Any, pos: Position): Unit = error(message, Some(pos))

}
