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
import java.io.IOException
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream
import java.util.Properties

import org.apache.commons.cli
import org.apache.commons.cli.Options
import org.apache.commons.cli.DefaultParser
import org.apache.commons.cli.CommandLine
import usbrubberducky.ast.PrettyPrinter
import usbrubberducky.lang.Lexer
import usbrubberducky.lang.Parser
import usbrubberducky.util.Context
import usbrubberducky.util.Pipeline
import usbrubberducky.util.StdoutPrinter

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try


object Main {
  val progname = "java -jar new-encoder.jar"

  /**
    * 0-9 range: General
    * 10-19 range: Bad input
    * 20-29 range: Internal errors (file bug report)
    */
  private object ExitCodes {
    val Success = 0
    val Failure = 1
    val BadCommandlineArguments = 10
    val InputFileFailed = 11
    val InvalidLayout = 12
    val LayoutFailed = 20
    val KeyboardFailed = 21
  }

  private case class Settings(
    help: Boolean = false,
    infile: Option[String] = None,
    outfile: Option[String] = None,
    layout: String = "us",
    prettyPrint: Boolean = false
  )

  private case class InputFile(fileName: String, source: Source)
  private case class Inputs(settings: Settings, input: InputFile, layout: Properties, keyboard: Properties)


  private object Options {
    val help: cli.Option = cli.Option.builder("h").longOpt("help").desc("Show this help and exit").build()
    val infile: cli.Option = cli.Option.builder("i").longOpt("infile").desc("DuckyScript file to encode (default: stdin)")
      .hasArg(true).argName("file").optionalArg(false).build()
    val layout: cli.Option = cli.Option.builder("l").longOpt("layout").desc("Keyboard layout name (default: us)")
      .hasArg(true).argName("file").optionalArg(false).build()
    val outfile: cli.Option = cli.Option.builder("o").longOpt("outfile").desc("File to write binary output to (default: stdout)")
      .hasArg(true).argName("file").optionalArg(false).build()
    val pretty: cli.Option = cli.Option.builder(null).longOpt("pretty").desc("Pretty-print DuckyScript to stdout instead of writing binary file (overrides --outfile)").build()

    val Options = new Options()
      .addOption(help)
      .addOption(infile)
      .addOption(layout)
      .addOption(outfile)
      .addOption(pretty)
  }

  private def processArguments(args: Array[String]): Try[Settings] = {
    Try (new DefaultParser().parse(Options.Options, args)) map { cmd =>
      if (cmd.getArgs.isEmpty == false) {
        err(
          s"Unknown or malformed command line option${if (cmd.getArgs.size > 1) "s" else ""}: ${cmd.getArgs mkString " "}"
        )
        sys.exit(ExitCodes.BadCommandlineArguments)
      }

      Settings(
        help = cmd hasOption Options.help.getOpt,
        infile = Option(cmd.getOptionValue(Options.infile.getOpt)),
        layout = Option(cmd.getOptionValue(Options.layout.getOpt)) getOrElse "us",
        outfile = Option(cmd.getOptionValue(Options.outfile.getOpt)),
        prettyPrint = cmd hasOption Options.pretty.getLongOpt
      )
    }
  }

  private def describeOptions(options: Seq[cli.Option], leftMargin: String = "  "): String = {
    def getNames(opt: cli.Option): String = {
      val name = (opt.getOpt, opt.getLongOpt) match {
        case (short, null) => s"-${short}"
        case (null, long) => s"    --${long}"
        case (short, long) => s"-${short}, --${long}"
      }

      if (opt.hasArg)
        if (opt.hasOptionalArg)
          s"${name} [${opt.getArgName}]"
        else
          s"${name} ${opt.getArgName}"
      else
        name
    }

    val longestName: Int = options.map(getNames(_).length).max

    options map { opt =>
      val name = getNames(opt)
      val paddingAmount: Int = longestName - name.length + 2
      val padding = List.fill(paddingAmount)(' ') mkString ""

      s"${leftMargin}${name}${padding}${opt.getDescription}"
    } mkString "\n"
  }

  private def sortOptions(options: Iterable[cli.Option]): Seq[cli.Option] =
    options.toList.sortBy { opt => Option(opt.getLongOpt) getOrElse opt.getOpt }


  private def printUsage(): Unit = {
    println(
      s"""Usage: ${progname} <options>
        |
        |Options:
        |${describeOptions(sortOptions(Options.Options.getOptions.asScala))}
      """.stripMargin)
  }

  private def err(message: String): Unit = Console.err.println("ERROR: " + message)

  private def loadProperties(resourceName: String): Try[Properties] = Try {
    val result = new Properties()
    result.load(getClass().getResourceAsStream(resourceName))
    result
  }

  private def runEncoder(inputs: Inputs, settings: Settings): Int = {
    val pipeline: Pipeline[Source, Try[Any]] = if (inputs.settings.prettyPrint) {
      Lexer andThen Parser andThen PrettyPrinter andThen StdoutPrinter
    } else {
      Lexer andThen Parser andThen NewEncoder andThen new Pipeline[List[Byte], Unit]() {
        override def run(ctx: Context)(bytes: List[Byte]): Unit = {
          val out: PrintStream = settings.outfile match {
            case Some("-") => new PrintStream(System.out)
            case Some(outFile) => new PrintStream(new FileOutputStream(new File(outFile)))
            case None => Console.out
          }

          bytes foreach { byte: Byte => out.write(byte) }
          out.flush()
        }
      }
    }

    val context = new Context(inputs.keyboard, inputs.layout, inputFileName = Some(inputs.input.fileName))
    pipeline.run(context)(inputs.input.source) match {
      case Success(_) => ExitCodes.Success
      case Failure(_) => ExitCodes.Failure
    }
  }

  def run(settings: Settings): Int = {
    if (settings.help) {
      printUsage()
      ExitCodes.Success
    } else {
      val inputFile = Try {
        settings.infile match {
          case Some(fileName) => InputFile(fileName, Source fromFile fileName)
          case None => InputFile("STDIN", Source.stdin)
        }
      } recover {
        case _: FileNotFoundException =>
          err(s"File not found: ${settings.infile}")
          sys.exit(ExitCodes.InputFileFailed)

        case _ =>
          err(s"Failed to open input file: ${settings.infile}")
          sys.exit(ExitCodes.InputFileFailed)
      }

      val layout = loadProperties(settings.layout + ".properties") recover {
        case _: NullPointerException =>
          err(s"Unknown layout: ${settings.layout}")
          sys.exit(ExitCodes.InvalidLayout)

        case error: IOException =>
          err(s"Failed to load layout definition: ${settings.layout} ($error)")
          sys.exit(ExitCodes.LayoutFailed)

        case error: IllegalArgumentException =>
          err(s"""Corrupted layout definition "${settings.layout}" - please file a bug report. ($error)""")
          sys.exit(ExitCodes.LayoutFailed)
      }

      val keyboard = loadProperties("keyboard.properties") recover {
        case error =>
          err(s"Failed to load keycode mapping - please file a bug report. ($error)")
          sys.exit(ExitCodes.KeyboardFailed)
      }

      (inputFile, layout, keyboard) match {
        case (Success(inputFile), Success(layout), Success(keyboard)) =>
          runEncoder(Inputs(settings, inputFile, layout, keyboard), settings)
        case _ => ExitCodes.Failure
      }
    }
  }

  def main(args: Array[String]) {
    val exitCode = processArguments(args) match {
      case Success(settings) => run(settings)
      case Failure(e) => {
        println("Error: " + e.getMessage)
        println()
        printUsage()
        ExitCodes.BadCommandlineArguments
      }
    }
    sys.exit(exitCode)
  }
}
