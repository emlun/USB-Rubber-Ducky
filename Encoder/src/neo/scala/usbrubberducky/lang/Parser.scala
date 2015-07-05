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
import util.NoPosition

import Tokens._
import ast.Trees._

object Parser extends Pipeline[Iterator[Token], Script] {

  override def run(ctx: Context)(tokens: Iterator[Token]) = {

    var bufferedToken: Option[Token] = None

    /**
     * Get the current token, or read and return the next token if the current
     * is undefined.
     */
    def currentToken(): Option[Token] = bufferedToken orElse readToken()

    /**
     * Read the next token into the current token buffer, and return it.
     */
    def readToken(): Option[Token] = {
      bufferedToken =
        if(tokens.hasNext) {
          tokens.next match {
            case OfKind(BAD) => readToken() // Skip bad tokens
            case goodToken   => Some(goodToken)
          }
        } else {
          None
        }
      bufferedToken
    }


    /**
     * Test if the current token is of the expected type. If it is, then unset
     * the current token buffer and pass the token as the argument to the thenn
     * function. If it is not, then produce an error message and do not run the
     * thenn function.
     */
    def eat[T](expected: TokenKind*)(thenn: Token => Option[T]): Option[T] =
      currentToken() orElse {
          ctx.reporter.error(s"Expected ${expected mkString " or "}, but reached end of input.")
          None
        } flatMap { token =>
          if(expected contains token.kind) {
            bufferedToken = None
            thenn(token)
          } else {
            ctx.reporter.error(s"Expected ${expected mkString " or "}, got ${token.kind}")
            None
          }
        }

    def eatIntLit[T](andThen: (IntLit => T)): Option[T] = eat(INTLITKIND) { token =>
        token match {
          case intLit: IntLit => Some(andThen(intLit))
          case _ => {
            ctx.reporter.error(s"Expected integer literal, got ${token.kind}", token.pos)
            None
          }
        }
      }

    def eatPosIntLit[T](andThen: (PosIntLit => T)): Option[T] = eat(POSINTLITKIND) { token =>
        token match {
          case intLit: PosIntLit => Some(andThen(intLit))
          case _ => {
            ctx.reporter.error(s"Expected positive integer literal, got ${token.kind}", token.pos)
            None
          }
        }
      }

    def eatStringLit[T](andThen: (StringLit => T)): Option[T] = eat(STRLITKIND) { token =>
        token match {
          case stringLit: StringLit => Some(andThen(stringLit))
          case _ => {
            ctx.reporter.error(s"Expected string literal, got ${token.kind}", token.pos)
            None
          }
        }
      }

    def maybeEatKeyName(): Option[KeyName] = eat(KEYNAMEKIND, NEWLINE) { maybeKeyName =>
        maybeKeyName match {
          case keyName: KeyName => Some(keyName)
          case _                => None
        }
      }

    def maybeEatRepeat(): Option[Repeat] = currentToken() match {
        case Some(OfKind(NEWLINE)) => {
          readToken()
          maybeEatRepeat()
        }
        case Some(OfKind(REPEAT)) => eat(REPEAT) { repeatToken =>
            eatIntLit { times => Repeat(times) }
          }
        case _ => None
      }

    def parseStatement(): Option[Statement] = eat(BEGIN_STATEMENT_TOKEN_KINDS.toList:_*) { token => token match {
          case OfKind(NEWLINE)        => None
          case keyName: KeyName       => Some(KeyPress(keyName, maybeEatRepeat()))
          case OfKind(ALT)            => Some(Alt(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(ALT_SHIFT)      => Some(AltShift(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(ALT_TAB)        => Some(AltTab(maybeEatRepeat(), token.pos))
          case OfKind(COMMAND)        => Some(Command(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(COMMAND_OPTION) => Some(CommandOption(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(CONTROL)        => Some(Ctrl(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(CTRL_ALT)       => Some(CtrlAlt(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(CTRL_SHIFT)     => Some(CtrlShift(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(SHIFT)          => Some(Shift(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(SUPER)          => Some(Super(maybeEatKeyName(), maybeEatRepeat(), token.pos))
          case OfKind(DELAY)          => eatIntLit    { intLit    => Delay(intLit, maybeEatRepeat()) }
          case OfKind(STRING)         => eatStringLit { stringLit => TypeString(stringLit, maybeEatRepeat()) }
          case _                      => None
        }
      }

    def parseStatements(): List[Statement] =
      if(tokens.hasNext) {
        val stmt = parseStatement()
        stmt ++: parseStatements()
      } else Nil

    def parseDefaultDelay(): Option[DefaultDelay] =
      currentToken() match {
          case Some(defaultDelay@OfKind(DEFAULTDELAY)) => {
            readToken()
            eatIntLit { intLit => DefaultDelay(intLit) }
          }
          case _                                 => None
        }

    Script(
      defaultDelay = parseDefaultDelay(),
      statements   = parseStatements()
    )
  }

}
