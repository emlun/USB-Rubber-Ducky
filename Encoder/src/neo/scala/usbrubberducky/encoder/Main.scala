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

object Main extends App {

  private case class Settings(
    infile: Option[String] = None,
    outfile: Option[String] = None,
    layout: Option[String] = None
  )

  private def processArguments(args: List[String], settings: Settings): Either[String, Settings] = args match {
      case Nil                     => Right(settings)
      case "-i" :: infile  :: tail => processArguments(tail, settings.copy(infile  = Some(infile)))
      case "-o" :: outfile :: tail => processArguments(tail, settings.copy(outfile = Some(outfile)))
      case "-l" :: layout  :: tail => processArguments(tail, settings.copy(layout  = Some(layout)))
      case head :: tail            => Left("Unknown command line option or too few option arguments: " + head)
    }

  println(processArguments(args.toList, Settings()))
}
