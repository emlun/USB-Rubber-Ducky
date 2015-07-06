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

import scala.collection.JavaConverters._
import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers

import lang.{Lexer,Parser}
import util.Context

import test._

class ComparisonTest extends FunSpec with Matchers with TestHelpers {

  val TEST_FILES =
    "src/test/resources/basic-terminal-commands-ubuntu.ducky" ::
    "src/test/resources/batch-wiper-drive-eraser.ducky" ::
    "src/test/resources/copy-file-to-desktop.ducky" ::
    "src/test/resources/create-wireless-network-association-auto-connect-pineapple.ducky" ::
    "src/test/resources/deny-net-access.ducky" ::
    "src/test/resources/dev-test-with-defaultdelay.ducky" ::
    "src/test/resources/dev-test-without-defaultdelay.ducky" ::
    "src/test/resources/disable-avg-2012.ducky" ::
    "src/test/resources/disable-avg-2013.ducky" ::
    "src/test/resources/download-mimikatz-grab-passwords-and-email-via-gmail.ducky" ::
    "src/test/resources/ducky-downloader.ducky" ::
    "src/test/resources/ducky-phisher.ducky" ::
    "src/test/resources/eicar-av-test.ducky" ::
    "src/test/resources/fork-bomb.ducky" ::
    "src/test/resources/ftp-download-upload.ducky" ::
    "src/test/resources/generic-batch.ducky" ::
    "src/test/resources/helloworld.ducky" ::
    "src/test/resources/hide-cmd-window-1.ducky" ::
    "src/test/resources/hide-cmd-window-2.ducky" ::
    "src/test/resources/hide-cmd-window-3.ducky" ::
    "src/test/resources/information-gathering-ubuntu.ducky" ::
    "src/test/resources/local-dns-poisoning.ducky" ::
    "src/test/resources/lock-your-computer-message.ducky" ::
    "src/test/resources/mimikatz-2.ducky" ::
    "src/test/resources/mimikatz.ducky" ::
    "src/test/resources/missdirection.ducky" ::
    "src/test/resources/mobiletabs.ducky" ::
    "src/test/resources/mrgrays-rubber-hacks.ducky" ::
    "src/test/resources/netcat-ftp-download-and-reverse-shell.ducky" ::
    "src/test/resources/non-malicious-auto-defacer.ducky" ::
    "src/test/resources/oneofeach-space-padded.ducky" ::
    "src/test/resources/oneofeach.ducky" ::
    "src/test/resources/osx-ascii-prank.ducky" ::
    "src/test/resources/osx-grab-minecraft-account-password-and-upload-to-ftp.ducky" ::
    "src/test/resources/osx-internet-protocol-slurp.ducky" ::
    "src/test/resources/osx-local-dns-poisoning.ducky" ::
    "src/test/resources/osx-passwordless-ssh-access.ducky" ::
    "src/test/resources/osx-photo-booth-prank.ducky" ::
    "src/test/resources/osx-root-backdoor.ducky" ::
    "src/test/resources/osx-user-backdoor.ducky" ::
    "src/test/resources/osx-wget-and-execute.ducky" ::
    "src/test/resources/osx-youtube-blaster.ducky" ::
    "src/test/resources/paint-hack.ducky" ::
    "src/test/resources/pineapple-association.ducky" ::
    "src/test/resources/powershell-wget-execute-hidden.ducky" ::
    "src/test/resources/powershell-wget-execute.ducky" ::
    "src/test/resources/remotely-possible.ducky" ::
    "src/test/resources/restart-prank.ducky" ::
    "src/test/resources/retrieve-sam-and-system-from-a-live-file-system-2.ducky" ::
    "src/test/resources/retrieve-sam-and-system-from-a-live-file-system.ducky" ::
    "src/test/resources/reverse-shell.ducky" ::
    "src/test/resources/run-java-from-sd.ducky" ::
    "src/test/resources/runexe-from-sd.ducky" ::
    "src/test/resources/silly-mouse-windows-is-for-kids-2.ducky" ::
    "src/test/resources/silly-mouse-windows-is-for-kids.ducky" ::
    "src/test/resources/ugly-rolled-prank.ducky" ::
    "src/test/resources/utilman-exploit.ducky" ::
    "src/test/resources/wallpaper-prank-2.ducky" ::
    "src/test/resources/wallpaper-prank.ducky" ::
    "src/test/resources/wifi-backdoor.ducky" ::
    "src/test/resources/wifun-v1.1.ducky" ::
    "src/test/resources/windows-screen-rotation-hack.ducky" ::
    "src/test/resources/xmas.ducky" ::
    "src/test/resources/you-got-quacked.ducky" ::
    "src/test/resources/youtube-roll.ducky" ::
    Nil

  describe("The new encoder") {

    describe("produces the same output as the old encoder") {

      TEST_FILES foreach { inputFile =>

        it(s"on $inputFile") {
          val oldBytes: List[Byte] = {
            val enc = new Encoder(inputFile, null, null)
            enc.setup()
            enc.encode().asScala.map(Byte.unbox(_)).toList
          }

          val newBytes: List[Byte] =
            (Lexer andThen Parser andThen NewEncoder).run(newContext)(Source fromFile inputFile)

          newBytes should be (oldBytes)
        }

      }

    }

  }
}
