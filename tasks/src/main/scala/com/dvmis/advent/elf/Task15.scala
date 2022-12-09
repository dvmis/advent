package com.dvmis.advent.elf

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class Task15 extends Task {
  import Task15._

  override def execute(): Unit = println(count(read()))
}

object Task15 {
  type Board = IndexedSeq[IndexedSeq[Field]]

  case class Field(height: Int, var visible: Boolean = false)

  def read(): Board = {
    val buffer = new ArrayBuffer[IndexedSeq[Field]]
    var line = StdIn.readLine()
    while (line != null) {
      buffer.addOne(line map (x => Field(x - '0')))
      line = StdIn.readLine()
    }
    buffer.toIndexedSeq
  }

  def count(board: Board): Int = {
    val size = board.length

    def checkDir(getField: Int => Field, start: Int, finish: Int, next: Int => Int): Unit = {
      var max = -1
      var j = start
      while (j != finish && max < 9) {
        val field = getField(j)
        if (field.height > max) {
          max = field.height
          if (!field.visible) field.visible = true
        }
        j = next(j)
      }
    }

    def checkIndices(indices: Range, getField: (Int, Int) => Field): Unit =
      for (i <- indices) {
        checkDir(getField(i, _), 0, size, _ + 1)
        checkDir(getField(i, _), size - 1, -1, _ - 1)
      }

    checkIndices(board.indices, (i, j) => board(i)(j))
    checkIndices(board(0).indices, (i, j) => board(j)(i))

    (for (i <- board.indices; j <- board.indices) yield board(i)(j)).count(_.visible)
  }
}
