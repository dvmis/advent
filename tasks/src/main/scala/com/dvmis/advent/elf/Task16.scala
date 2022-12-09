package com.dvmis.advent.elf

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class Task16 extends Task {
  import Task16._

  override def execute(): Unit = println(maxScore(read()))
}

object Task16 {
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

  def maxScore(board: Board): Int =
    (for (i <- 1 until (board.length - 1); j <- 1 until (board(i).length - 1)) yield score(board, i, j)).max

  type MoveGenerator = (Int, Int) => (Int, Int)

  def score(board: Board, i: Int, j: Int): Int = {
    val origin = board(i)(j)

    def outOfRange(x: Int): Boolean = x < 0 || x >= board.length

    @tailrec
    def scoreDir(mover: MoveGenerator, k: Int = i, l: Int = j, score: Int = 0): Int = {
      val (newK, newL) = mover(k, l)
      if (outOfRange(newK) || outOfRange(newL)) score
      else if (board(newK)(newL).height >= origin.height) score + 1
      else scoreDir(mover, newK, newL, score + 1)
    }

    scoreDir((x, y) => (x + 1, y)) *
      scoreDir((x, y) => (x - 1, y)) *
      scoreDir((x, y) => (x, y + 1)) *
      scoreDir((x, y) => (x, y - 1))
  }
}
