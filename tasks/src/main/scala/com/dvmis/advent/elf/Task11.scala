package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task11.eval

import scala.io.StdIn

class Task11 extends Task {
  override def execute(): Unit = println(s"Result == ${eval(4)}")
}

class Task12 extends Task {
  override def execute(): Unit = println(s"Result == ${eval(14)}")
}

object Task11 {
  def eval(n: Int): Int = StdIn.readLine().sliding(n).indexWhere(_.distinct.length == n) + n
}
