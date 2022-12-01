package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task2.{MaxElves, MaxItems}
import com.dvmis.advent.elf.model.Elf

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

class Task2(elves: ElfSource) extends Task {

  override def execute(): Unit = {
    val result = findMax(MaxItems.of(3))
    println(s"Sum = ${result.items.map(_.nCalories).sum}; items = ${result.items}")
  }

  @tailrec
  private def findMax(max: MaxElves, idx: Int = 0): MaxElves =
    elves.nextElf(idx) match {
      case Some(e) => findMax(max :+ e, idx + 1)
      case None => max
    }
}

object Task2 {
  class MaxItems[A](size: Int, val items: TreeSet[A]) {
    def :+(x: A): MaxItems[A] = new MaxItems[A](size, items + x take size)
  }

  object MaxItems {
    def of[A: Ordering](size: Int): MaxItems[A] = new MaxItems(size, TreeSet.empty)
  }

  type MaxElves = MaxItems[Elf]
}
