package com.dvmis.advent.elf

import scala.annotation.tailrec

class Task1(elves: ElfSource) extends Task {

  override def execute(): Unit = println(s"Max elf = ${findMax()}")

  @tailrec
  private def findMax(index: Int = 0, max: Option[Elf] = None): Option[Elf] =
    elves.nextElf(index) match {
      case Some(e) => findMax(index + 1, if (max exists (_.nCalories >= e.nCalories)) max else Some(e))
      case None => max
    }
}
