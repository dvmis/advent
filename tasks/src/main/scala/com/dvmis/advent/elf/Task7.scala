package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task7.eval

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

class Task7 extends Task {
  override def execute(): Unit = {
    val result = eval((r1, r2) => (r1 contains r2) || (r2 contains r1))
    println(s"Result = $result")
  }
}

class Task8 extends Task {
  override def execute(): Unit = {
    val result = eval(_ overlaps _)
    println(s"Result = $result")
  }
}

object Task7 {
  case class IntRange(from: Int, to: Int) {
    def contains(other: IntRange): Boolean = from <= other.from && to >= other.to
    def contains(x: Int): Boolean = from <= x && x <= to
    def overlaps(other: IntRange): Boolean =
      (List(other.from, other.to) exists contains) || (List(from, to) exists other.contains)
  }

  @tailrec
  def eval(f: (IntRange, IntRange) => Boolean, res: Int = 0): Int =
    readLine() match {
      case Some(range) => if (f.tupled(range)) eval(f, res + 1) else eval(f, res)
      case None => res
    }

  def readLine(): Option[(IntRange, IntRange)] =
    Option(StdIn.readLine()) flatMap (l => Try(parseRanges(l)).toOption)

  def parseRanges(s: String): (IntRange, IntRange) = {
    val Array(x1, y1, x2, y2) = s.split("[-,]") map (_.toInt)
    (IntRange(x1, y1), IntRange(x2, y2))
  }
}
