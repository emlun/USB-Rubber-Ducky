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
import util.Reporter
import util.Trimmed

import Tokens._

object Lexer extends Pipeline[Source, Iterator[Token]] {

  def ignoreLine(line: String): Boolean = line.trim.isEmpty || (line.trim startsWith "REM")

  def suggestCommands(reporter: Reporter)(attempt: String): Unit = {
    val suggestions = KEYWORD_TOKEN_KINDS filter { _ startsWith attempt }
    if(!suggestions.isEmpty) {
      reporter.info(
        "Did you mean any of the following? " + (suggestions flatMap (_.keywords) mkString ", ")
      )
    }
  }

  override def run(ctx: Context)(source: Source) =
    (source.getLines.zipWithIndex flatMap (processLine(ctx) _).tupled).toIterator

  def processLine(ctx: Context)(line: String, lineIndex: Int): List[Token] =
    if(ignoreLine(line)) {
      Nil
    } else {
      val linePos = Position(lineIndex + 1, 1, line, fileName = ctx.inputFileName)
      val newline = new Token(NEWLINE, linePos.copy(column = line.length))

      line.split(" ", 2) match {
        case Array(Trimmed(word))               => processSingleWord(ctx, linePos, newline)(word)
        case Array(Trimmed(head), tail: String) => processCommandWithArgument(ctx, linePos, newline)(head, tail)
      }
    }

  def processSingleWord
      (ctx: Context, linePos: Position, newline: Token)
      (word: String)
      : List[Token] = {
    val keywordKindCandidates = KEYWORD_TOKEN_KINDS filter { _ matches word }

    keywordKindCandidates.toList match {
      case List(commandKind: KeywordKind) => new Token(commandKind, linePos) :: newline :: Nil
      case Nil                            =>
        if(KEYNAMEKIND matches word) {
          KeyName(word, linePos) :: newline :: Nil
        } else {
          ctx.reporter.error("Only one word given, but is not a command or key name.", linePos)
          suggestCommands(ctx.reporter)(word)
          new Token(BAD, linePos) :: Nil
        }
    }
  }

  def processCommandWithArgument
      (ctx: Context, linePos: Position, newline: Token)
      (command: String, argument: String)
      : List[Token] = {
    val keywordKindCandidates = KEYWORD_TOKEN_KINDS filter { _ matches command }
    val argumentPos = linePos.copy(column = command.length + 2)

    keywordKindCandidates.toList match {
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
        suggestCommands(ctx.reporter)(command)
        new Token(BAD, linePos) :: Nil
      }
    }
  }

}
