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

  // Original Author:Jason Appelbaum Jason@Hak5.org
  private def charToBytes(keyboard: Properties, layout: Properties)(c: Char): List[Byte] =
    codeToBytes(keyboard, layout)(charToCode(c))

  // Original Author:Jason Appelbaum Jason@Hak5.org
  private def charToCode(c: Char): String = (c match {
      case _ if c < 128 => "ASCII"
      case _ if c < 256 => "ISO_8859_1"
      case _            => "UNICODE"
    }) + "_" + Integer.toHexString(c).toUpperCase

  // Original Author:Jason Appelbaum Jason@Hak5.org
  private def codeToBytes(keyboard: Properties, layout: Properties)(str: String): List[Byte] = {
    if(layout.getProperty(str) != null) {
      val keys: List[String] = (layout.getProperty(str).split(",") map { _.trim }).toList
      keys map { key =>
          if(keyboard.getProperty(key) != null) {
            strToByte(keyboard.getProperty(key).trim())
          } else if(layout.getProperty(key) != null) {
            strToByte(layout.getProperty(key).trim())
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
  private def strToByte(str: String): Byte = str match {
      case _ if str startsWith "0x" => Integer.parseInt(str.substring(2), 16).toByte
      case _                        => Integer.parseInt(str).toByte
    }

  private def strInstrToByte(keyboard: Properties, layout: Properties)(instruction: String): Byte = {
    val recurse: (String => Byte) = strInstrToByte(keyboard, layout)

    if(keyboard.getProperty("KEY_" + instruction) != null) {
      strToByte(keyboard.getProperty("KEY_" + instruction))
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
      case _                => charToBytes(keyboard, layout)(instruction.charAt(0)).head
    }
  }

  private def encodeDelay(milliseconds: Int): List[Byte] =
    List.fill(milliseconds / 255)(List(0x00.toByte, 0xFF.toByte)).flatten ++:
    (milliseconds % 255 match {
      case 0         => Nil
      case remainder => List(0x00.toByte, remainder.toByte)
    }) ++:
    Nil

  private def encodeModifiedKeypress(keyboard: Properties, layout: Properties, defaultDelayBytes: List[Byte])
                                    (modifierName: String, keyName: String) = List(
      strInstrToByte(keyboard, layout)(keyName),
      strToByte(keyboard.getProperty(modifierName))
    ) ++: defaultDelayBytes

  private def encodeModifierKeypress(keyboard: Properties, layout: Properties, defaultDelayBytes: List[Byte])
                                    (modifierKeyName: String) =
    List(strToByte(keyboard.getProperty(modifierKeyName)), 0x00: Byte) ++: defaultDelayBytes

  private def encodeStatement(ctx: Context, defaultDelay: Option[DefaultDelay])
                             (statement: Statement): List[Byte] = {
    val defaultDelayBytes = defaultDelay map { delay => encodeDelay(delay.milliseconds.value) } getOrElse Nil
    val encodeModified = encodeModifiedKeypress(ctx.keyboard, ctx.layout, defaultDelayBytes) _
    val encodeModifier = encodeModifierKeypress(ctx.keyboard, ctx.layout, defaultDelayBytes) _

    statement match {
      case Delay(IntLit(milliseconds, _)) => encodeDelay(milliseconds)

      case TypeString(StringLit(value, _)) =>
        value.flatMap({ c: Char =>
          val bytes = charToBytes(ctx.keyboard, ctx.layout)(c)
          bytes ++: (if(bytes.length % 2 == 0) Nil else List(0x00: Byte))
        }) ++:
        defaultDelayBytes

      case Ctrl(None, _)                              => encodeModifier("KEY_LEFT_CTRL")
      case Ctrl(Some(KeyPress(KeyName(value, _))), _) => encodeModified("MODIFIERKEY_CTRL", value)

      case Alt(None, _)                              => encodeModifier("KEY_LEFT_ALT")
      case Alt(Some(KeyPress(KeyName(value, _))), _) => encodeModified("MODIFIERKEY_ALT", value)

      case Shift(None, _)                              => encodeModifier("KEY_LEFT_SHIFT")
      case Shift(Some(KeyPress(KeyName(value, _))), _) => encodeModified("MODIFIERKEY_SHIFT", value)

      case CtrlAlt(None, pos) => {
        ctx.reporter.warn("CTRL-ALT-nothing does nothing.", pos)
        Nil
      }
      case CtrlAlt(Some(KeyPress(KeyName(value, _))), _) =>
        List(
          strInstrToByte(ctx.keyboard, ctx.layout)(value),
          (
            strToByte(ctx.keyboard.getProperty("MODIFIERKEY_CTRL")) |
            strToByte(ctx.keyboard.getProperty("MODIFIERKEY_ALT"))
          ).toByte
        ) ++: defaultDelayBytes
    }
  }

  override def run(ctx: Context)(script: Script) = {

    val bytes: List[Byte] = script.statements flatMap encodeStatement(ctx, script.defaultDelay) _

    bytes
  }

}
