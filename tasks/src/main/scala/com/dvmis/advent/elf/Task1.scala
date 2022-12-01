package com.dvmis.advent.elf

import com.dvmis.advent.elf.model.Elf

import scala.annotation.tailrec

class Task1(elves: ElfSource) extends Task {

  override def execute(): Unit = {
    val result = findMax()
    println(s"Max elf = $result")
  }

  @tailrec
  private def findMax(idx: Int = 0, max: Option[Elf] = None): Option[Elf] =
    elves.nextElf(idx) match {
      case Some(e) => findMax(idx + 1, (e :: max.toList).maxOption)
      case None => max
    }
}
