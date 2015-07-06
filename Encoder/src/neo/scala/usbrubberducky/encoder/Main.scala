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

import java.io.FileNotFoundException
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

object Main {

  /**
   * 0-9 range: General
   * 10-19 range: Bad input
   * 20-29 range: Internal errors (file bug report)
   */
  private object ExitCodes {
    val success = 0
    val failure = 1
    val badCommandlineArguments = 10
    val inputFileFailed = 11
    val invalidLayout = 12
    val layoutFailed = 20
    val keyboardFailed = 21
  }

  private case class Settings(
    infile: Option[String] = None,
    outfile: Option[String] = None,
    layout: String = "us",
    prettyPrint: Boolean = false
  )

  private case class InputFile(fileName: String, source: Source)
  private case class Inputs(settings: Settings, input: InputFile, layout: Properties, keyboard: Properties)

  private def processArguments(args: List[String], settings: Settings): Settings = args match {
      case Nil                     => settings
      case "-i" :: infile  :: tail => processArguments(tail, settings.copy(infile  = Some(infile)))
      case "-o" :: outfile :: tail => processArguments(tail, settings.copy(outfile = Some(outfile)))
      case "-l" :: layout  :: tail => processArguments(tail, settings.copy(layout  = layout))
      case "--pretty" :: tail      => processArguments(tail, settings.copy(prettyPrint = true))
      case head :: tail            => {
          err("Unknown command line option or too few option arguments: " + head)
          sys.exit(ExitCodes.badCommandlineArguments)
        }
    }

  private def err(message: String): Unit = Console.err.println("ERROR: " + message)

  private def loadProperties(resourceName: String): Try[Properties] = Try({
      val result = new Properties()
      result.load(getClass().getResourceAsStream(resourceName))
      result
    })

  private def runEncoder(inputs: Inputs): Int = {
    val pipeline = if(inputs.settings.prettyPrint) {
        Lexer andThen Parser andThen PrettyPrinter andThen StdoutPrinter
      } else {
        Lexer andThen Parser andThen NewEncoder andThen new Pipeline[List[Byte], Unit]() {
          override def run(ctx: Context)(bytes: List[Byte]): Unit = {
            bytes foreach { byte: Byte => Console.out.write(byte) }
            Console.out.flush()
          }
        }
      }

      val context = new Context(inputs.keyboard, inputs.layout, inputFileName = Some(inputs.input.fileName))
      pipeline.run(context)(inputs.input.source)

      if(context.reporter.hasErrors) {
        ExitCodes.failure
      } else {
        ExitCodes.success
      }
    }

  def main(args: Array[String]) {
    val settings = processArguments(args.toList, Settings())

    val inputFile = Try(settings.infile match {
          case Some(fileName) => InputFile(fileName, Source fromFile fileName)
          case None           => InputFile("STDIN",  Source.stdin)
        }) recover {
          case _: FileNotFoundException => {
            err(s"File not found: ${settings.infile}")
            sys.exit(ExitCodes.inputFileFailed)
          }
          case _ => {
            err(s"Failed to open input file: ${settings.infile}")
            sys.exit(ExitCodes.inputFileFailed)
          }
        }

    val layout = loadProperties(settings.layout + ".properties") recover {
        case _: NullPointerException => {
          err(s"Unknown layout: ${settings.layout}")
          sys.exit(ExitCodes.invalidLayout)
        }
        case error: IOException => {
          err(s"Failed to load layout definition: ${settings.layout} ($error)")
          sys.exit(ExitCodes.layoutFailed)
        }
        case error: IllegalArgumentException => {
          err(s"""Corrupted layout definition "${settings.layout}" - please file a bug report. ($error)""")
          sys.exit(ExitCodes.layoutFailed)
        }
      }

    val keyboard = loadProperties("keyboard.properties") recover {
        case error => {
          err(s"Failed to load keycode mapping - please file a bug report. ($error)")
          sys.exit(ExitCodes.keyboardFailed)
        }
      }

    val exitCode = (inputFile, layout, keyboard) match {
      case (Success(inputFile), Success(layout), Success(keyboard)) =>
        runEncoder(Inputs(settings, inputFile, layout, keyboard))
      case _ => ExitCodes.failure
    }

    sys.exit(exitCode)
  }
}
