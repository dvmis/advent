package com.dvmis.advent.elf

object TaskExecutor extends App {
  val task: Task = new Task2(ElfSource())

  task.execute()
}

trait Task {
  def execute(): Unit
}
