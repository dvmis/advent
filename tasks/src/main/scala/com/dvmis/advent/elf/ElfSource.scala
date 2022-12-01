package com.dvmis.advent.elf

import com.dvmis.advent.elf.model.Elf

import scala.annotation.tailrec
import scala.io.StdIn

trait ElfSource {
  def nextElf(id: Int): Option[Elf]
}

object ElfSource {
  def apply(): ElfSource = StdInElfSource

  object StdInElfSource extends ElfSource {
    private final val ElfDelim = ""

    override def nextElf(id: Int): Option[Elf] = getNext(id, None)

    @tailrec
    private def getNext(id: Int, nCal: Option[Int]): Option[Elf] =
      (Option(StdIn.readLine()) map (_.trim), nCal) match {
        case (Some(ElfDelim) | None, Some(c)) => Some(Elf(id, c))
        case (Some(s), _) =>
          val xs = (s.toIntOption.filter(_ > 0) :: nCal :: Nil).flatten
          getNext(id, if (xs.nonEmpty) Some(xs.sum) else None)
        case _ => None
      }
  }
}
