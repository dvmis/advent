package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task18._

import scala.annotation.tailrec
import scala.io.StdIn

class Task18 extends Task {
  override def execute(): Unit = {
    println("rope(2) = " + eval(2))
    println("rope(10) = " + eval(10))
  }

  def eval(n: Int): Int = tailPath(Rope(List.fill(n)(Point(0, 0))), readMoves()).distinct.size
}

object Task18 {
  case class Point(x: Int, y: Int)
  case class Rope(points: List[Point])
  case class Move(dx: Int, dy: Int)

  def tailPath(rope: Rope, moves: List[Move]): List[Point] =
    moves.foldLeft(rope :: Nil)((xs, x) => doMove(xs.head, x) :: xs) map (_.points.last)

  def doMove(rope: Rope, move: Move): Rope = {
    val head :: other = rope.points
    Rope(other.foldLeft(head.copy(x = head.x + move.dx, y = head.y + move.dy) :: Nil) { (points, p) =>
      val Point(prevX, prevY) = points.head
      val newPoint = if (Math.abs(prevX - p.x) < 2 && Math.abs(prevY - p.y) < 2) p
                     else Point(p.x + Integer.signum(prevX - p.x), p.y + Integer.signum(prevY - p.y))
      newPoint :: points
    }.reverse)
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
