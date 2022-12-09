package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task17._

import scala.annotation.tailrec
import scala.io.StdIn

class Task17 extends Task {
  override def execute(): Unit = println(tailPath(Rope(Point(0, 0), Point(0, 0)), readMoves()).distinct.size)
}

object Task17 {
  case class Point(x: Int, y: Int)

  case class Rope(head: Point, tail: Point)

  case class Move(dx: Int, dy: Int)

  def tailPath(rope: Rope, moves: List[Move]): List[Point] =
    moves.foldLeft(rope :: Nil)((xs, x) => doMove(xs.head, x) :: xs) map (_.tail)

  def doMove(rope: Rope, move: Move): Rope = {
    val head = rope.head.copy(x = rope.head.x + move.dx, y = rope.head.y + move.dy)
    if (Math.abs(head.x - rope.tail.x) < 2 && Math.abs(head.y - rope.tail.y) < 2)
      Rope(head, rope.tail)
    else
      Rope(head, rope.head)
  }

  def parseLine(line: String): List[Move] = {
    val Array(dir, n) = line.trim.split("\\s+")
    val move = dir match {
      case "L" => Move(-1, 0)
      case "R" => Move(1, 0)
      case "U" => Move(0, 1)
      case "D" => Move(0, -1)
    }
    List.fill(n.toInt)(move)
  }

  @tailrec
  def readMoves(moves: List[Move] = Nil): List[Move] =
    StdIn.readLine() match {
      case null => moves.reverse
      case s => readMoves(parseLine(s) ::: moves)
    }
}
