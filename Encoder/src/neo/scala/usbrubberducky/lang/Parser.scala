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

    def currentToken(): Option[Token] = bufferedToken orElse readToken()

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

    def eat[T](expected: TokenKind*)(thenn: Token => Option[T]): Option[T] =
      currentToken() flatMap { token =>
        if(expected contains token.kind) {
          bufferedToken = None
          thenn(token)
        } else {
          ctx.reporter.error(s"Expected ${expected mkString " or "}, got ${token.kind}")
          None
        }
      } orElse {
        ctx.reporter.error(s"Expected ${expected mkString " or "}, but reached end of input.")
        None
      }

    def readIntLit[T](andThen: (IntLit => T)): Option[T] = eat(INTLITKIND) { token =>
        token match {
          case intLit: IntLit => Some(andThen(intLit))
          case _ => {
            ctx.reporter.error(s"Expected integer literal, got ${token.kind}", token.pos)
            None
          }
        }
      }

    def parseDefaultDelay(): Option[DefaultDelay] =
      currentToken() match {
          case Some(defaultDelay@OfKind(DEFAULTDELAY)) => {
            readToken()
            readIntLit { intLit => DefaultDelay(intLit) }
          }
          case _                                 => None
        }

    Script(
      defaultDelay = parseDefaultDelay(),
      statements   = Nil
    )
  }

}
