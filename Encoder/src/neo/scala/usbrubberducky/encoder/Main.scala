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

import java.io.InputStream
import java.io.IOException
import java.util.Properties

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import ast.Trees.Script
import ast.PrettyPrinter

import lang.{Lexer,Parser}
import util.Context
import util.Pipeline
import util.Reporter
import util.StdoutPrinter

object Main extends App {

  private object ExitCodes {
    val success = 0
    val badCommandlineArguments = 1
    val invalidLayout = 2
    val layoutFailed = 3
  }

  private case class Settings(
    infile: Option[String] = None,
    outfile: Option[String] = None,
    layout: String = "us",
    prettyPrint: Boolean = false
  )

  private def processArguments(args: List[String], settings: Settings): Either[String, Settings] = args match {
      case Nil                     => Right(settings)
      case "-i" :: infile  :: tail => processArguments(tail, settings.copy(infile  = Some(infile)))
      case "-o" :: outfile :: tail => processArguments(tail, settings.copy(outfile = Some(outfile)))
      case "-l" :: layout  :: tail => processArguments(tail, settings.copy(layout  = layout))
      case "--pretty" :: tail      => processArguments(tail, settings.copy(prettyPrint = true))
      case head :: tail            => Left("Unknown command line option or too few option arguments: " + head)
    }

  private def err(message: String, exitCode: Int): Int = {
    Console.err.println("ERROR: " + message)
    exitCode
  }

  val exitCode : Int = processArguments(args.toList, Settings()) match {
    case Left(errorMessage) => err(errorMessage, ExitCodes.badCommandlineArguments)
    case Right(settings)    => {
      val pipeline = settings match {
        case Settings(_, _, _, true) => Lexer andThen Parser andThen PrettyPrinter andThen StdoutPrinter
        case _                       => Lexer andThen Parser andThen NewEncoder
      }

      Try(
          getClass().getResourceAsStream(settings.layout + ".properties")
        ) map { stream: InputStream =>
          val result = new Properties()
          result.load(stream)
          result
        } match {
          case Success(layout: Properties) => {
            val (fileName: String, source: Source) = settings.infile match {
                case Some(fileName) => (fileName, Source fromFile fileName)
                case None           => ("STDIN",  Source.stdin)
              }

            val context = new Context(layout = layout, inputFileName = Some(fileName))
            pipeline.run(context)(source)
            ExitCodes.success
          }
          case Failure(error) => error match {
              case _: NullPointerException => err(s"Unknown layout: ${settings.layout}", ExitCodes.invalidLayout)
              case error: IOException =>
                err(s"Failed to load layout definition: ${settings.layout} ($error)", ExitCodes.layoutFailed)
              case error: IllegalArgumentException =>
                err(s"Corrupted layout definition: ${settings.layout} ($error)", ExitCodes.layoutFailed)
            }
        }
    }
  }

  System.exit(exitCode)
}
