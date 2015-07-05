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
import lang.Tokens.KeyName

object Printer {

  private def keyNameOptionToString(key: Option[KeyName]): String = key map { " " + _.value } getOrElse ""

  def prettyPrint(script: Script): String =
    (script.defaultDelay map { tree => s"DEFAULT_DELAY ${tree.milliseconds.value}\n" } getOrElse "") +
    (script.statements map { statement =>
      statement match {
        case KeyPress(keyName) => keyName.value

        case Alt(key, _)           => "ALT"            + keyNameOptionToString(key)
        case AltShift(key, _)      => "ALT-SHIFT"      + keyNameOptionToString(key)
        case AltTab( _)            => "ALT-TAB"
        case Command(key, _)       => "COMMAND"        + keyNameOptionToString(key)
        case CommandOption(key, _) => "COMMAND-OPTION" + keyNameOptionToString(key)
        case Ctrl(key, _)          => "CTRL"           + keyNameOptionToString(key)
        case CtrlAlt(key, _)       => "CTRL-ALT"       + keyNameOptionToString(key)
        case CtrlShift(key, _)     => "CTRL-SHIFT"     + keyNameOptionToString(key)
        case Shift(key, _)         => "SHIFT"          + keyNameOptionToString(key)
        case Super(key, _)         => "WINDOWS"        + keyNameOptionToString(key)

        case Delay(milliseconds) => "DELAY "  + milliseconds.value
        case Repeat(times)       => "REPEAT " + times.value
        case TypeString(value)   => "STRING " + value.value
      }
    } mkString "\n")

}

object PrettyPrinter extends Pipeline[Script, String]() {
  override def run(ctx: Context)(script: Script) = Printer.prettyPrint(script)
}
