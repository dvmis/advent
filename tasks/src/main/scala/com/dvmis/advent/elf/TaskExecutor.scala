package com.dvmis.advent.elf

object TaskExecutor extends App {
  val task: Task = new Task14

  task.execute()
}

trait Task {
  def execute(): Unit
}
