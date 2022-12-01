package com.dvmis.advent.elf

import scala.annotation.tailrec
import scala.io.StdIn

trait ElfSource {
  def nextElf(index: Int, nCal: Option[Int] = None): Option[Elf]
}

object ElfSource {
  def apply(): ElfSource = StdInElfSource

  object StdInElfSource extends ElfSource {
    private final val EndOfElf = ""

    @tailrec
    override def nextElf(index: Int, nCal: Option[Int] = None): Option[Elf] =
      (Option(StdIn.readLine()) map (_.trim), nCal) match {
        case (Some(EndOfElf) | None, Some(c)) => Some(Elf(index, c))
        case (Some(s), _) =>
          val items = (s.toIntOption.filter(_ > 0) :: nCal :: Nil).flatten
          nextElf(index, if (items.nonEmpty) Some(items.sum) else None)
        case _ => None
      }
  }
}
