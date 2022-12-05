package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task9.ops
import com.dvmis.advent.elf.Task9.ops.doMove

import scala.annotation.tailrec
import scala.io.StdIn

class Task9 extends Task {
  override def execute(): Unit = {
    val result = ops.eval(doMove(_, _, reverse = true))
    println(s"Result = $result")
  }
}

class Task10 extends Task {
  override def execute(): Unit = {
    val result = ops.eval(doMove(_, _, reverse = false))
    println(s"Result = $result")
  }
}

object Task9 {
  object model {
    case class SwapTask(gate: Gate, moves: Seq[Move])
    type Gate = IndexedSeq[Stack]
    type Stack = List[Char]
    case class Move(from: Int, to: Int, number: Int)
  }

  object ops {
    import model._

    type MoveStrategy = (Gate, Move) => Gate

    def eval(mover: MoveStrategy): String =
      evalTask(mover, readTask()) flatMap (_.headOption) mkString ""

    @tailrec
    def evalTask(mover: MoveStrategy, task: SwapTask): Gate = task.moves match {
      case Nil => task.gate
      case head :: tail => evalTask(mover, task.copy(gate = mover(task.gate, head), moves = tail))
    }

    def doMove(gate: Gate, move: Move, reverse: Boolean): Gate = {
      val (xs, ys) = gate(move.from) splitAt move.number
      gate updated(move.from, ys) updated(move.to, (if (reverse) xs.reverse else xs) ::: gate(move.to))
    }

    def readTask(): SwapTask = SwapTask(readGate(Map.empty), readMoves(Nil))

    @tailrec
    def readGate(stacks: Map[Int, List[Char]]): Gate =
      Option(StdIn.readLine()) match {
        case Some(s) if s contains '[' =>
          readGate(
            s.zipWithIndex
              .filter(_._1.isUpper)
              .foldLeft(stacks) {
                case (acc, (c, i)) =>
                  val idx = (i - 1) / 4
                  acc + (idx -> (c :: acc.getOrElse(idx, Nil)))
              })
        case _ =>
          for (i <- 0 to stacks.keys.max) yield stacks.getOrElse(i, Nil).reverse
      }

    final val MoveLine = "move (\\d+) from (\\d+) to (\\d+)".r

    @tailrec
    def readMoves(moves: List[Move]): Seq[Move] =
      Option(StdIn.readLine()) match {
        case Some(MoveLine(n, from, to)) => readMoves(Move(from.toInt - 1, to.toInt - 1, n.toInt) :: moves)
        case Some(_) => readMoves(moves)
        case None => moves.reverse
      }
  }
}
