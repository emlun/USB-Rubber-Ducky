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
import util.TryPipeline
import util.Position
import util.Reporter
import util.Trimmed

import Tokens._

object Lexer extends TryPipeline[Source, Iterator[Token]] {

  override def tryRun(ctx: Context)(source: Source) = {
    val result = source.getLines.zipWithIndex flatMap (processLine(ctx) _).tupled
    if (ctx.reporter.hasErrors) {
      throw new RuntimeException("DuckyScript syntax error(s) in Lexer")
    } else {
      result.toIterator
    }
  }

  def ignoreLine(line: String): Boolean = line.trim.isEmpty || (line.trim startsWith "REM")

  def suggestCommands(reporter: Reporter)(attempt: String): Unit = {
    val suggestions = KeywordTokenKinds filter { _ startsWith attempt }
    if (!suggestions.isEmpty) {
      reporter.info(
        "Did you mean any of the following? " + (suggestions flatMap (_.keywords) mkString ", ")
      )
    }
  }

  def processLine(ctx: Context)(line: String, lineIndex: Int): List[Token] =
    if (ignoreLine(line)) {
      Nil
    } else {
      val linePos = Position(lineIndex + 1, 1, line, fileName = ctx.inputFileName)
      val newline = new Token(NEWLINE, linePos.copy(column = line.length))

      line.split(" ", 2) match {
        case Array(Trimmed(word))       => List(processSingleWord(ctx, linePos)(word), newline)
        case Array(Trimmed(head), tail) => processCommandWithArgument(ctx, linePos)(head, tail) ++: List(newline)
      }
    }

  def processSingleWord
      (ctx: Context, linePos: Position)
      (word: String)
      : Token = {
    val keywordKindCandidates = KeywordTokenKinds filter { _ matches word }

    keywordKindCandidates.toList match {
      case List(commandKind: KeywordKind) => new Token(commandKind, linePos)
      case Nil                            =>
        if (KEYNAMEKIND matches word) {
          KeyName(word, linePos)
        } else {
          ctx.reporter.error(s"Not a command or key name: $word", linePos)
          suggestCommands(ctx.reporter)(word)
          new Token(BAD, linePos)
        }
    }
  }

  def processCommandWithArgument
      (ctx: Context, linePos: Position)
      (commandString: String, argumentString: String)
      : List[Token] = {
    val command = processSingleWord(ctx, linePos)(commandString)
    val argumentPos = linePos.copy(column = commandString.length + 2)

    val argument = command.kind match {
        case STRING                        => StringLit(argumentString, argumentPos)
        case DELAY | DEFAULTDELAY =>
          if (INTLITKIND matches argumentString.trim) {
            IntLit(argumentString.trim.toInt, argumentPos)
          } else {
            ctx.reporter.error("Bad integer literal: " + argumentString, argumentPos)
            new Token(BAD, argumentPos)
          }
        case REPEAT =>
          if (POSINTLITKIND matches argumentString.trim) {
            PosIntLit(argumentString.trim.toInt, argumentPos)
          } else {
            ctx.reporter.error("Bad positive integer literal: " + argumentString, argumentPos)
            new Token(BAD, argumentPos)
          }
        case _ =>
          if (KEYNAMEKIND matches argumentString.trim) {
            KeyName(argumentString.trim, argumentPos)
          } else if (argumentString.trim.isEmpty) {
            new Token(NEWLINE, linePos.copy(column = (commandString + argumentString).length + 1))
          } else {
            ctx.reporter.error(s"Expected key name or newline, got: ${argumentString.trim}", argumentPos)
            new Token(BAD, argumentPos)
          }
      }

    List(command, argument)
  }

}
