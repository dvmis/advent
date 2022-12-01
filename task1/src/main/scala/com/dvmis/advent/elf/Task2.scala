package com.dvmis.advent.elf

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

class Task2(elves: ElfSource) extends Task {

  override def execute(): Unit = {
    val result = findMax(MaxItems.of(3))
    println(s"Sum = ${result.items.map(_.nCalories).sum}; items = ${result.items}")
  }

  implicit object ElfOrdering extends Ordering[Elf] {
    override def compare(x: Elf, y: Elf): Int = Ordering[Int].compare(y.nCalories, x.nCalories)
  }

  class MaxItems(n: Int, val items: TreeSet[Elf]) {
    def :+(x: Elf): MaxItems = new MaxItems(n, items + x take n)
  }

  object MaxItems {
    def of(n: Int): MaxItems = new MaxItems(n, TreeSet.empty)
  }

  @tailrec
  private def findMax(items: MaxItems, index: Int = 0): MaxItems =
    elves.nextElf(index) match {
      case Some(e) => findMax(items :+ e, index + 1)
      case None => items
    }
}
