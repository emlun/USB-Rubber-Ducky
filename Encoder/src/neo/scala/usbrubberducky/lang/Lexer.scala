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
package lang

import scala.io.Source

import util.Context
import util.Pipeline
import util.Position
import util.Trimmed

import Tokens._

object Lexer extends Pipeline[Source, Iterator[Token]] {

  val COMMAND_TOKEN_KINDS: Set[TokenKind] = Set(
    DEFAULTDELAY, DELAY, REPEAT,
    ALT, ALT_SHIFT, ALT_TAB, COMMAND, COMMAND_OPTION, CONTROL, CTRL_ALT, CTRL_SHIFT, SHIFT, STRING, SUPER
  )

  def isLineComment(line: String): Boolean = line.trim startsWith "REM"

  override def run(ctx: Context)(source: Source) =
    (source.getLines.zipWithIndex flatMap (processLine(ctx) _).tupled).toIterator

  def processLine(ctx: Context)(line: String, lineIndex: Int): List[Token] =
    if(isLineComment(line)) {
      Nil
    } else {
      val linePos = Position(lineIndex + 1, 1, line, fileName = ctx.inputFileName)
      val newline = new Token(NEWLINE, linePos.copy(column = line.length))

      line.split(" ", 2) match {
        case Array(Trimmed(commandOrKeyName))      => processSingleWord(ctx, linePos, newline)(commandOrKeyName)
        case Array(Trimmed(command), tail: String) => processCommandWithArgument(ctx, linePos, newline)(command, tail)
      }
    }

  def processSingleWord
      (ctx: Context, linePos: Position, newline: Token)
      (commandOrKeyName: String)
      : List[Token] = {
    val commandKindCandidates = COMMAND_TOKEN_KINDS filter { _ matches commandOrKeyName }

    commandKindCandidates.toList match {
      case List(commandKind: KeywordKind) => new Token(commandKind, linePos) :: newline :: Nil
      case Nil                            =>
        if(KEYNAMEKIND matches commandOrKeyName) {
          KeyName(commandOrKeyName, linePos) :: newline :: Nil
        } else {
          ctx.reporter.error("Only one word given, but is not a command or key name.", linePos)
          new Token(BAD, linePos) :: Nil
        }
    }
  }

  def processCommandWithArgument
      (ctx: Context, linePos: Position, newline: Token)
      (command: String, argument: String)
      : List[Token] = {
    val commandKindCandidates = COMMAND_TOKEN_KINDS filter { _ matches command }
    val argumentPos = linePos.copy(column = command.length + 2)

    commandKindCandidates.toList match {
      case List(commandKind: KeywordKind) =>
        new Token(commandKind, linePos) ::
        (commandKind match {
          case STRING                        => StringLit(argument, argumentPos)
          case DELAY | DEFAULTDELAY | REPEAT =>
            if(INTLITKIND matches argument.trim) {
              IntLit(argument.trim.toInt, argumentPos)
            } else {
              ctx.reporter.error("Bad integer literal: " + argument, argumentPos)
              new Token(BAD, argumentPos)
            }
          case _                             => KeyName(argument.trim, argumentPos)
        }) ::
        newline ::
        Nil
      case _                              => {
        ctx.reporter.error("Unknown or ambiguous command: " + command, linePos)
        val suggestions = COMMAND_TOKEN_KINDS filter { _ startsWith command }
        if(!suggestions.isEmpty) {
          ctx.reporter.info("Did you mean any of the following? " + (suggestions mkString ", "))
        }
        new Token(BAD, linePos) :: Nil
      }
    }
  }

}
