/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  (C)  2015-2016  Christian Krause                                                  *
 *                                                                                               *
 *  Christian Krause  <christian.krause@mailbox.org>                                             *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  This file is part of strace-analyzer.                                                        *
 *                                                                                               *
 *  strace-analyzer is free software: you can redistribute it and/or modify it under the terms   *
 *  of the GNU General Public License as published by the Free Software Foundation, either       *
 *  version 3 of the License, or any later version.                                              *
 *                                                                                               *
 *  strace-analyzer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; *
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
 *  See the GNU General Public License for more details.                                         *
 *                                                                                               *
 *  You should have received a copy of the GNU General Public License along with                 *
 *  strace-analyzer. If not, see <http://www.gnu.org/licenses/>.                                 *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


package strace
package analyze

trait HasFileOpSummary {
  case class FileOpSummary(bytes: Long, ops: Long, reqbytes: Long, seconds: Double) {
    def +(that: FileOpSummary): FileOpSummary = FileOpSummary (
      bytes = this.bytes + that.bytes,
      ops = this.ops + that.ops,
      reqbytes = this.reqbytes + that.reqbytes,
      seconds = this.seconds + that.seconds
    )

    def bps: Double = bytes / seconds
    def bpo: Double = bytes.toDouble / ops
    def hBytes: String = Memory.humanize(bytes)
    def hSeconds: String = Duration.humanize(seconds)
    def hbps: String = Memory.humanize(bps.round)
    def hbpo: String = Memory.humanize(bpo.round)
    def areqbytes: Double = reqbytes.toDouble / ops
    def hreqbytes: String = Memory.humanize(areqbytes.round)

    def humanized(op: String): String =
      s"""$op $hBytes in $hSeconds (~ $hbps / s) with $ops ops (~ $hbpo / op, ~ $hreqbytes request size)"""
  }

  object FileOpSummary {
    val empty = FileOpSummary(bytes = 0L, ops = 0L, reqbytes = 0L, seconds = 0.0)

    def apply(entry: LogEntry with HasBytes): FileOpSummary =
      FileOpSummary(bytes = entry.bytes, ops = 1, reqbytes = entry.reqbytes, seconds = entry.time.toDouble)
  }
}
