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

import java.util.Properties

import ast.Trees._
import lang.Tokens._
import util.Context
import util.Pipeline


object NewEncoder extends Pipeline[Script, List[Byte]] {

  override def run(ctx: Context)(script: Script) = {

    // Original Author:Jason Appelbaum Jason@Hak5.org
    def charToBytes(c: Char): List[Byte] = codeToBytes(charToCode(c))

    // Original Author:Jason Appelbaum Jason@Hak5.org
    def charToCode(c: Char): String = (c match {
        case _ if c < 128 => "ASCII"
        case _ if c < 256 => "ISO_8859_1"
        case _            => "UNICODE"
      }) + "_" + Integer.toHexString(c).toUpperCase

    // Original Author:Jason Appelbaum Jason@Hak5.org
    def codeToBytes(str: String): List[Byte] = {
      if(ctx.layout.getProperty(str) != null) {
        val keys: List[String] = (ctx.layout.getProperty(str).split(",") map { _.trim }).toList
        keys map { key =>
            if(ctx.keyboard.getProperty(key) != null) {
              strToByte(ctx.keyboard.getProperty(key).trim())
            } else if(ctx.layout.getProperty(key) != null) {
              strToByte(ctx.layout.getProperty(key).trim())
            } else {
              println("Key not found: " + key)
              0x00: Byte
            }
          }
      } else {
        println("Char not found:"+str);
        List(0x00: Byte)
      }
    }

    // Original Author:Jason Appelbaum Jason@Hak5.org
    def strToByte(str: String): Byte = str match {
        case _ if str startsWith "0x" => Integer.parseInt(str.substring(2), 16).toByte
        case _                        => Integer.parseInt(str).toByte
      }

    def strInstrToByte(instruction: String): Byte = {
      val recurse: (String => Byte) = strInstrToByte _

      if(ctx.keyboard.getProperty("KEY_" + instruction) != null) {
        strToByte(ctx.keyboard.getProperty("KEY_" + instruction))
      } else instruction match {
        case "ESCAPE"         => recurse("ESC")
        case "DEL"            => recurse("DELETE")
        case "BREAK"          => recurse("PAUSE")
        case "CONTROL"        => recurse("CTRL")
        case "DOWNARROW"      => recurse("DOWN")
        case "UPARROW"        => recurse("UP")
        case "LEFTARROW"      => recurse("LEFT")
        case "RIGHTARROW"     => recurse("RIGHT")
        case "MENU"           => recurse("APP")
        case "WINDOWS"        => recurse("GUI")
        case "PLAY" | "PAUSE" => recurse("MEDIA_PLAY_PAUSE")
        case "STOP"           => recurse("MEDIA_STOP")
        case "MUTE"           => recurse("MEDIA_MUTE")
        case "VOLUMEUP"       => recurse("MEDIA_VOLUME_INC")
        case "VOLUMEDOWN"     => recurse("MEDIA_VOLUME_DEC")
        case "SCROLLLOCK"     => recurse("SCROLL_LOCK")
        case "NUMLOCK"        => recurse("NUM_LOCK")
        case "CAPSLOCK"       => recurse("CAPS_LOCK")
        case _                => charToBytes(instruction.charAt(0)).head
      }
    }

    def encodeDelay(milliseconds: Int): List[Byte] =
      List.fill(milliseconds / 255)(List(0x00.toByte, 0xFF.toByte)).flatten ++:
      (milliseconds % 255 match {
        case 0         => Nil
        case remainder => List(0x00.toByte, remainder.toByte)
      }) ++:
      Nil

    def encodeModifiedKeypress(keyName: String, modifierNames: String*) = List(
        strInstrToByte(keyName),
        modifierNames.foldLeft(0: Byte) { (byte: Byte, modifierName: String) =>
          (byte | strToByte(ctx.keyboard.getProperty(modifierName))).toByte
        }
      )

    def encodeModifierKeypress(modifierKeyName: String) =
      List(strToByte(ctx.keyboard.getProperty(modifierKeyName)), 0x00: Byte)

    def encodeStatement(statement: Statement): List[Byte] = {
      val defaultDelayBytes = script.defaultDelay map { delay => encodeDelay(delay.milliseconds.value) } getOrElse Nil
      (statement match {
        case Delay(IntLit(milliseconds, _)) => encodeDelay(milliseconds)

        case TypeString(StringLit(value, _)) =>
          value.flatMap({ c: Char =>
            val bytes = charToBytes(c)
            bytes ++: (if(bytes.length % 2 == 0) Nil else List(0x00: Byte))
          })

        case Ctrl(None, _)                              => encodeModifierKeypress("KEY_LEFT_CTRL")
        case Ctrl(Some(KeyPress(KeyName(value, _))), _) => encodeModifiedKeypress(value, "MODIFIERKEY_CTRL")

        case Alt(None, _)                              => encodeModifierKeypress("KEY_LEFT_ALT")
        case Alt(Some(KeyPress(KeyName(value, _))), _) => encodeModifiedKeypress(value, "MODIFIERKEY_ALT")

        case Shift(None, _)                              => encodeModifierKeypress("KEY_LEFT_SHIFT")
        case Shift(Some(KeyPress(KeyName(value, _))), _) => encodeModifiedKeypress(value, "MODIFIERKEY_SHIFT")

        case CtrlAlt(None, pos) => {
          ctx.reporter.warn("CTRL-ALT-nothing does nothing.", pos)
          Nil
        }
        case CtrlAlt(Some(KeyPress(KeyName(value, _))), _) =>
          encodeModifiedKeypress(value, "MODIFIERKEY_CTRL", "MODIFIERKEY_ALT")

        case CtrlShift(None, pos) => {
          ctx.reporter.warn("CTRL-ALT-nothing does nothing.", pos)
          Nil
        }
        case CtrlShift(Some(KeyPress(KeyName(value, _))), _) =>
          encodeModifiedKeypress(value, "MODIFIERKEY_CTRL", "MODIFIERKEY_SHIFT")

        case CommandOption(None, pos) => {
          ctx.reporter.warn("COMMAND-OPTION-nothing does nothing.", pos)
          Nil
        }
        case CommandOption(Some(KeyPress(KeyName(value, _))), _) =>
          encodeModifiedKeypress(value, "MODIFIERKEY_LEFT_GUI", "MODIFIERKEY_ALT")

        case AltShift(None, _) =>
          encodeModifiedKeypress("LEFT_ALT", "MODIFIERKEY_LEFT_ALT", "MODIFIERKEY_SHIFT")
        case AltShift(Some(KeyPress(KeyName(value, _))), _) =>
          encodeModifiedKeypress(value, "MODIFIERKEY_LEFT_ALT", "MODIFIERKEY_SHIFT")

        case AltTab(_) => encodeModifiedKeypress("TAB", "MODIFIERKEY_LEFT_ALT")

        case Super(None, _) => encodeModifierKeypress("MODIFIERKEY_LEFT_GUI")
        case Super(Some(KeyPress(KeyName(value, _))), _) => encodeModifiedKeypress(value, "MODIFIERKEY_LEFT_GUI")
      }) ++: (statement match {
        case TypeString(_) | Ctrl(_,_) | Alt(_,_) | Shift(_,_) | CtrlAlt(Some(_),_) | CtrlShift(Some(_), _) | CommandOption(Some(_), _) | AltShift(_,_) | AltTab(_) | Super(_,_) => defaultDelayBytes
        case _ => Nil
      })
    }

    val bytes: List[Byte] = script.statements flatMap encodeStatement _

    bytes
  }

}
