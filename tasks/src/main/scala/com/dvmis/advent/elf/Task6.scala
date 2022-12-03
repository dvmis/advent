package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task6.eval

import scala.annotation.tailrec
import scala.io.StdIn

class Task6 extends Task {
  override def execute(): Unit = println(s"Result = ${eval(3)}")
}

object Task6 {
  @tailrec
  final def eval(size: Int, sum: Int = 0): Int = {
    val xs = readGroup(size)
    if (xs.size < size) sum else eval(size, sum + evalGroup(xs))
  }

  def readGroup(n: Int): Seq[String] = Seq.fill(n)(readLine()).flatten

  def readLine(): Option[String] = Option(StdIn.readLine()) map (_.trim)

  def evalGroup(xs: Seq[String]): Int = (xs map (_.toSet) reduce (_ intersect _) map weightOf).sum

  def weightOf(c: Char): Int = if (c.isLower) c - 'a' + 1 else if (c.isUpper) c - 'A' + 27 else 0
}
