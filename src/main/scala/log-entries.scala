package strace
package analyze

sealed abstract class LogEntry {
  def epoch: String
  def status: Int
  def time: String
}

object LogEntry {

  val fsSep = sys props "file.separator"

  case class Close(epoch: String, fd: String, status: Int, time: String) extends LogEntry

  object Close {
    val regex = """(\d+\.\d+) close\((\d+)\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Close] = line match {
      case regex(epoch, fd, status, time) =>
        Some(new Close(epoch, fd, status.toInt, time))
      case _ => None
    }
  }

  case class Creat(epoch: String, file: String, fd: String, time: String) extends LogEntry {
    def status = fd.toInt
  }

  object Creat {
    val regex = """(\d+\.\d+) creat\("([^"]+)", .+\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Creat] = line match {
      case regex(epoch, file, status, time) =>
        Some(new Creat(epoch, file, status, time))
      case _ => None
    }
  }

  case class Dup(epoch: String, oldFd: String, newFd: String, time: String) extends LogEntry {
    def status = newFd.toInt
  }

  object Dup {
    val regex = """(\d+\.\d+) dup\((\d+)\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Dup] = line match {
      case regex(epoch, oldFd, newFd, time) =>
        Some(new Dup(epoch, oldFd, newFd, time))
      case _ => None
    }
  }

  case class Open(epoch: String, file: String, status: Int, time: String) extends LogEntry {
    def fd = status.toString
  }

  object Open {
    val regex = """(\d+\.\d+) open\("([^"]+)", .+\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Open] = line match {
      case regex(epoch, file, status, time) =>
        Some(new Open(epoch, file, status.toInt, time))
      case _ => None
    }
  }

  case class OpenAt(epoch: String, wherefd: String, filename: String, status: Int, time: String) extends LogEntry {
    def file(path: String) = s"""$path$fsSep$filename"""
    def fd = status.toString
  }

  object OpenAt {
    val regex = """(\d+\.\d+) openat\((\d+|AT_FDCWD), "([^"]+)", .+\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[OpenAt] = line match {
      case regex(epoch, wherefd, file, status, time) =>
        Some(new OpenAt(epoch, wherefd, file, status.toInt, time))
      case _ => None
    }
  }

  case class Pipe(epoch: String, read: String, write: String, status: Int, time: String) extends LogEntry

  object Pipe {
    val regex = """(\d+\.\d+) pipe\(\[(\d+), (\d+)\]\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Pipe] = line match {
      case regex(epoch, read, write, status, time) =>
        Some(new Pipe(epoch, read, write, status.toInt, time))
      case _ => None
    }
  }

  case class Read(epoch: String, fd: String, bytes: Long, time: String) extends LogEntry {
    def status = bytes.toInt
  }

  object Read {
    val regex = """(\d+\.\d+) read\((\d+), .+\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Read] = line match {
      case regex(epoch, fd, bytes, time) =>
        Some(new Read(epoch, fd, bytes.toLong, time))
      case _ => None
    }
  }

  case class Write(epoch: String, fd: String, bytes: Long, time: String) extends LogEntry {
    def status = bytes.toInt
  }

  object Write {
    val regex = """(\d+\.\d+) write\((\d+), .+\)\s+= (\d+) <(\d+\.\d+)>""".r
    def unapply(line: String): Option[Write] = line match {
      case regex(epoch, fd, bytes, time) =>
        Some(new Write(epoch, fd, bytes.toLong, time))
      case _ => None
    }
  }

}