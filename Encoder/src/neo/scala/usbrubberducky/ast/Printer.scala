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

import util.Context
import util.Pipeline

import Trees._

object Printer {

  private def keyPressOptionToString(key: Option[KeyPress]): String =
    key map { " " + _.keyName.value } getOrElse ""

  def prettyPrint(script: Script): String = {
    val defdel = script.defaultDelay map { tree => s"DEFAULT_DELAY ${tree.delay.value}" } getOrElse ""
    defdel + "\n" + (script.statements map { statement =>
      statement match {
        case KeyPress(keyName) => keyName.value

        case Alt(key, _)           => "ALT"            + keyPressOptionToString(key)
        case AltShift(key, _)      => "ALT-SHIFT"      + keyPressOptionToString(key)
        case AltTab( _)            => "ALT-TAB"
        case Command(key, _)       => "COMMAND"        + keyPressOptionToString(key)
        case CommandOption(key, _) => "COMMAND-OPTION" + keyPressOptionToString(key)
        case Ctrl(key, _)          => "CTRL"           + keyPressOptionToString(key)
        case CtrlAlt(key, _)       => "CTRL-ALT"       + keyPressOptionToString(key)
        case CtrlShift(key, _)     => "CTRL-SHIFT"     + keyPressOptionToString(key)
        case Shift(key, _)         => "SHIFT"          + keyPressOptionToString(key)
        case Super(key, _)         => "WINDOWS"        + keyPressOptionToString(key)

        case Delay(milliseconds) => "DELAY "  + milliseconds.value
        case Repeat(times)       => "REPEAT " + times.value
        case TypeString(value)   => "STRING " + value.value
      }
    } mkString "\n")
  }

}

object PrettyPrinter extends Pipeline[Script, String]() {
  override def run(ctx: Context)(script: Script) = Printer.prettyPrint(script)
}
