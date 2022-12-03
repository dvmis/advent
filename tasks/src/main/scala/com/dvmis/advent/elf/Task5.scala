package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task5.eval

import scala.annotation.tailrec
import scala.io.StdIn

class Task5 extends Task {
  override def execute(): Unit = println(s"Result = ${eval(0)}")
}

object Task5 {
  @tailrec
  final def eval(sum: Int): Int =
    readLine() match {
      case Some(line) => eval(sum + evalLine(line))
      case None => sum
    }

  def readLine(): Option[String] = Option(StdIn.readLine()) map (_.trim)

  def evalLine(line: String): Int = {
    val (l, r) = line.splitAt(line.length / 2)
    ((l.toSet intersect r.toSet) map weightOf).sum
  }

  def weightOf(c: Char): Int = if (c.isLower) c - 'a' + 1 else if (c.isUpper) c - 'A' + 27 else 0
}
