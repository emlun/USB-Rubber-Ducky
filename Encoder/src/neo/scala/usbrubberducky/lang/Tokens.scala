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

import util.Position
import util.Positioned

object Tokens {

  val CommandTokenKinds: Set[KeywordKind] = Set(
    DELAY,
    ALT, ALT_SHIFT, ALT_TAB, COMMAND, COMMAND_OPTION, CONTROL, CTRL_ALT, CTRL_SHIFT, SHIFT, STRING, SUPER
  )

  val BeginStatementTokenKinds: Set[TokenKind] = CommandTokenKinds ++ Set(KEYNAMEKIND, NEWLINE)
  val KeywordTokenKinds: Set[KeywordKind] = CommandTokenKinds + DEFAULTDELAY + REPEAT

  sealed class Token(val kind: TokenKind, val pos: Position) extends Positioned {
    override def toString = kind.toString
  }

  sealed abstract class ValueToken[T](kind: TokenKind, pos: Position) extends Token(kind, pos) {
    def value: T
    override def toString = s"${getClass().getSimpleName}($value)"
  }

  sealed trait TokenKind {
    def startsWith(prefix: String): Boolean
    def matches(value: String): Boolean
  }
  sealed abstract class KeywordKind(val keywords: String*) extends TokenKind {
    override def startsWith(prefix: String) = keywords.find(_.startsWith(prefix)).isDefined
    override def matches(value: String)     = keywords contains value
  }

  object OfKind {
    def unapply(token: Token): Option[TokenKind] = Some(token.kind)
  }

  case object BAD extends TokenKind {
    override def startsWith(prefix: String) = false
    override def matches(prefix: String)    = false
  }

  case object NEWLINE extends TokenKind {
    override def startsWith(prefix: String) = false
    override def matches(value: String)     = false
  }

  case object ALT            extends KeywordKind("ALT")
  case object ALT_SHIFT      extends KeywordKind("ALT-SHIFT")
  case object ALT_TAB        extends KeywordKind("ALT-TAB")
  case object COMMAND        extends KeywordKind("COMMAND")
  case object COMMAND_OPTION extends KeywordKind("COMMAND-OPTION")
  case object CONTROL        extends KeywordKind("CONTROL", "CTRL")
  case object CTRL_ALT       extends KeywordKind("CTRL-ALT")
  case object CTRL_SHIFT     extends KeywordKind("CTRL-SHIFT")
  case object DEFAULTDELAY   extends KeywordKind("DEFAULTDELAY", "DEFAULT_DELAY")
  case object DELAY          extends KeywordKind("DELAY")
  case object REPEAT         extends KeywordKind("REPEAT")
  case object SHIFT          extends KeywordKind("SHIFT")
  case object STRING         extends KeywordKind("STRING")
  case object SUPER          extends KeywordKind("GUI", "WINDOWS")

  case object KEYNAMEKIND extends TokenKind {
    override def startsWith(prefix: String) = prefix matches "[a-zA-Z_0-9]*"
    override def matches(value: String)     = value  matches "[a-zA-Z_0-9]+"
    override def toString = "Key name"
  }
  case object INTLITKIND extends TokenKind {
    override def startsWith(prefix: String) = prefix.isEmpty || (prefix matches "0|[1-9][0-9]*")
    override def matches(value: String)     = value matches "0|[1-9][0-9]*"
    override def toString = "Integer literal"
  }
  case object POSINTLITKIND extends TokenKind {
    override def startsWith(prefix: String) = prefix.isEmpty || (prefix matches "[1-9][0-9]*")
    override def matches(value: String)     = value matches "[1-9][0-9]*"
    override def toString = "Positive integer literal"
  }
  case object STRLITKIND extends TokenKind {
    override def startsWith(prefix: String) = !(prefix contains "\n")
    override def matches(value: String)     = this startsWith value
    override def toString = "String literal"
  }

  case class KeyName(override val value: String, override val pos: Position)
    extends ValueToken[String](KEYNAMEKIND, pos)

  case class IntLit(override val value: Int, override val pos: Position)
    extends ValueToken[Int](INTLITKIND, pos)

  case class PosIntLit(override val value: Int, override val pos: Position)
    extends ValueToken[Int](POSINTLITKIND, pos) {
      if (value < 1) throw new IllegalArgumentException(s"PosIntLit value must be positive (got $value).")
    }

  case class StringLit(override val value: String, override val pos: Position)
    extends ValueToken[String](STRLITKIND, pos)

}
