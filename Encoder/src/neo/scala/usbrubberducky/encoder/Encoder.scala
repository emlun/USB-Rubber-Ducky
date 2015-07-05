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

      case TypeString(StringLit(value, _)) =>
        value.flatMap({ c: Char =>
          val bytes = charToBytes(ctx.keyboard, ctx.layout)(c)
          bytes ++: (if(bytes.length % 2 == 0) Nil else List(0x00: Byte))
        }) ++:
        defaultDelayBytes
    }
  }

  override def run(ctx: Context)(script: Script) = {

    val bytes: List[Byte] = script.statements flatMap encodeStatement(ctx, script.defaultDelay) _

    bytes
  }

}
