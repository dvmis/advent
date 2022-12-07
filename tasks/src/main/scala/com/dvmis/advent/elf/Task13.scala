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

    def evalSum(maxSize: NodeSize): NodeSize = findDirs(readFs(), Vector.empty, maxSize)._1.map(_._2).sum

    def evalMin(storageSize: NodeSize, requiredSize: NodeSize): NodeSize = {
      val (dirs, totalSize) = findDirs(readFs(), Vector.empty, Long.MaxValue)
      val sizeToFree = requiredSize - (storageSize - totalSize)
      (dirs map (_._2) filter (_ >= sizeToFree)).min
    }

    def findDirs(node: Node, parentPath: Path, maxSize: NodeSize): (Seq[(Path, NodeSize)], NodeSize) =
      node.info match {
        case FileInfo(size) => (Nil, size)
        case DirInfo(entries) =>
          val path = parentPath :+ node.name
          val (results, size) = entries.map(findDirs(_, path, maxSize))
            .foldLeft((Vector.empty[(Path, NodeSize)], 0L)) { case ((xs, x), (ys, y)) => (xs ++ ys, x + y) }
          (results :+ (path, size), size)
      }
  }

  object parser {
    import model._

    type DirsMap = Map[String, Seq[DirEntry]]
    case class DirEntry(name: String, size: Option[Long])

    object command {
      val CdRoot: Regex = "\\$ cd /".r
      val CdUp: Regex  = "\\$ cd \\.\\.".r
      val CdDown: Regex  = "\\$ cd (.+)".r

      val Ls: Regex = "\\$ ls".r
    }

    object output {
      val File: Regex = "([\\d]+) (.+)".r
      val Dir: Regex = "dir (.+)".r
    }

    def readFs(): Node = mkNode(parse(new InputReader, Vector.empty, Map.empty))

    def mkNode(dirs: DirsMap, path: String = ""): Node =
      FsNode(path.split("/").last, DirInfo(
        dirs(path) map {
          case DirEntry(name, Some(size)) => FsNode(name, FileInfo(size))
          case DirEntry(name, None) => mkNode(dirs, if (path.nonEmpty) s"$path/$name" else name)
        }))

    @tailrec
    def parse(reader: InputReader, path: Vector[String], dirs: DirsMap): DirsMap =
      reader.getLine match {
        case Some(command.CdRoot()) => parse(reader.next(), Vector.empty, dirs)
        case Some(command.CdUp()) => parse(reader.next(), path dropRight 1, dirs)
        case Some(command.CdDown(dirname)) => parse(reader.next(), path :+ dirname, dirs)
        case Some(command.Ls()) =>
          val (r, xs) = readDir(reader.next())
          parse(r, path, dirs + ((path mkString "/", xs)))
        case _ => dirs
      }

    @tailrec
    def readDir(reader: InputReader, result: Seq[DirEntry] = Vector.empty): (InputReader, Seq[DirEntry]) =
      reader.getLine match {
        case Some(output.File(size, name)) => readDir(reader.next(), result :+ DirEntry(name, Some(size.toLong)))
        case Some(output.Dir(name)) => readDir(reader.next(), result :+ DirEntry(name, None))
        case _ => (reader, result)
      }

    class InputReader {
      lazy val getLine: Option[String] = Option(StdIn.readLine())
      def next(): InputReader = new InputReader
    }
  }
}
