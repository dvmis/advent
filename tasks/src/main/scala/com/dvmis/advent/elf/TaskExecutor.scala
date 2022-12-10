package com.dvmis.advent.elf

object TaskExecutor extends App {
  val task: Task = new Day10

  task.execute()
}

trait Task {
  def execute(): Unit
}
