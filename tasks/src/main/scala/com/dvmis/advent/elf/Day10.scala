package com.dvmis.advent.elf

import com.dvmis.advent.elf.Day10._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex

class Day10 extends Task {
  override def execute(): Unit = {
    // task1()
    task2()
  }

  def task1(): Unit = {
    val states = evalChecks(List(20, 60, 100, 140, 180, 220))
    val result = (states map (s => s.nCycle * s.regX)).sum
    println(result)
  }

  def task2(): Unit = {
    val width = 40
    val height = 6

    evalChecks((1 to (width * height)).toList)
      .map(s => if (Math.abs(s.regX - ((s.nCycle - 1) % width)) < 2) "#" else ".")
      .sliding(width, width)
      .foreach(xs => println(xs.mkString))
  }
}

object Day10 {
  case class State(nCycle: Int, regX: Int)
  case class Command(dt: Int, dx: Int)

  @tailrec
  def evalChecks(checks: List[Int], state: State = State(1, 1), snapshots: List[State] = Nil): List[State] = {
    (checks, nextCmd()) match {
      case (Nil, _) | (_, None) => snapshots.reverse
      case (head :: tail, Some(cmd)) =>
        val newState = executeCmd(state, cmd)
        if (newState.nCycle > head) {
          val (skipped, other) = tail span (_ < newState.nCycle)
          evalChecks(other, newState, (skipped.length to 0 by -1).map(i => State(head + i, state.regX)).toList ::: snapshots)
        } else
          evalChecks(checks, newState, snapshots)
    }
  }

  def executeCmd(state: State, cmd: Command): State = State(state.nCycle + cmd.dt, state.regX + cmd.dx)

  final val AddX: Regex = "addx ([-\\d]+)".r
  final val Noop: Regex = "noop".r

  def nextCmd(): Option[Command] =
    Option(StdIn.readLine()) collect {
      case AddX(dx) => Command(2, dx.toInt)
      case Noop() => Command(1, 0)
    }
}