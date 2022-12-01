package com.dvmis.advent.elf

object model {
  case class Elf(id: Int, nCalories: Int)

  implicit object ElfOrdering extends Ordering[Elf] {
    override def compare(x: Elf, y: Elf): Int = Ordering[Int].compare(y.nCalories, x.nCalories)
  }
}
