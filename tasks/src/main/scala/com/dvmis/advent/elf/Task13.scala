package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task13.behavior._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex

class Task13 extends Task {
  override def execute(): Unit = println(evalSum(maxSize = 100000L))
}

class Task14 extends Task {
  override def execute(): Unit = println(evalMin(storageSize = 70000000L, requiredSize = 30000000L))
}

object Task13 {
  object model {
    case class FsNode[+A <: NodeInfo](name: String, info: A)

    sealed trait NodeInfo
    case class FileInfo(size: NodeSize) extends NodeInfo
    case class DirInfo(entries: Seq[FsNode[NodeInfo]]) extends NodeInfo

    type Node = FsNode[NodeInfo]
    type File = FsNode[FileInfo]
    type Dir = FsNode[DirInfo]

    type NodeSize = Long
    type Path = Seq[String]
  }

  object behavior {
    import model._
    import parser._

    def evalSum(maxSize: NodeSize): NodeSize =
      (findDirs(readFs(), Vector.empty)._1 collect { case (_, size) if size <= maxSize => size }).sum

    def evalMin(storageSize: NodeSize, requiredSize: NodeSize): NodeSize = {
      val (dirs, totalSize) = findDirs(readFs(), Vector.empty)
      val sizeToFree = requiredSize - (storageSize - totalSize)
      (dirs map (_._2) filter (_ >= sizeToFree)).min
    }

    def findDirs(node: Node, parentPath: Path): (Seq[(Path, NodeSize)], NodeSize) =
      node.info match {
        case FileInfo(size) => (Nil, size)
        case DirInfo(entries) =>
          val path = parentPath :+ node.name
          val (results, size) = entries.map(findDirs(_, path))
            .foldLeft((Vector.empty[(Path, NodeSize)], 0L)) { case ((xs, x), (ys, y)) => (xs ++ ys, x + y) }
          (results :+ (path, size), size)
      }
  }

  object parser {
    import model._

    type DirsMap = Map[String, Seq[DirEntry]]

    case class DirEntry(name: String, size: Option[Long])

    object command {
      val Cd: Regex = "\\$ cd ([^\\s]+)".r
      val Ls: Regex = "\\$ ls".r
    }

    object output {
      val File: Regex = "([\\d]+) (.+)".r
      val Dir: Regex = "dir (.+)".r
    }

    def readFs(): Node = mkNode(runCommand(new InputReader, Vector.empty, Map.empty))

    def mkNode(dirs: DirsMap, path: String = ""): Node =
      FsNode(path.split("/").last, DirInfo(
        dirs(path) map {
          case DirEntry(name, Some(size)) => FsNode(name, FileInfo(size))
          case DirEntry(name, None) => mkNode(dirs, if (path.nonEmpty) s"$path/$name" else name)
        }))

    @tailrec
    def runCommand(reader: InputReader, path: Vector[String], dirs: DirsMap): DirsMap = {
      import command._

      reader.getLine match {
        case Some(Cd(dirname)) => runCommand(reader.next(), changePath(path, dirname), dirs)
        case Some(Ls()) =>
          val (r, es) = readEntries(reader.next())
          runCommand(r, path, dirs + ((path mkString "/", es)))
        case _ => dirs
      }
    }

    def changePath(path: Vector[String], dirname: String): Vector[String] =
      dirname match {
        case "/" => Vector.empty
        case ".." => path dropRight 1
        case _ => path :+ dirname
      }

    @tailrec
    def readEntries(reader: InputReader, entries: Seq[DirEntry] = Vector.empty): (InputReader, Seq[DirEntry]) = {
      import output._

      reader.getLine match {
        case Some(File(size, name)) => readEntries(reader.next(), entries :+ DirEntry(name, Some(size.toLong)))
        case Some(Dir(name)) => readEntries(reader.next(), entries :+ DirEntry(name, None))
        case _ => (reader, entries)
      }
    }

    class InputReader {
      lazy val getLine: Option[String] = Option(StdIn.readLine())
      def next(): InputReader = new InputReader
    }
  }
}
