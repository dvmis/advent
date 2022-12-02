package com.dvmis.advent.elf

import com.dvmis.advent.elf.Task3.{evalRound, nextRound}

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

class Task3 extends Task {
  override def execute(): Unit = {
    val result = doExecute(0)
    println(s"Result = $result")
  }

  @tailrec
  private def doExecute(result: Int): Int =
    nextRound() match {
      case Some(r) => doExecute(result + r.you.score + evalRound(r).score)
      case None => result
    }
}

object Task3 {
  sealed abstract class Subject(val score: Int) {
    def wins: Subject = this match {
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper
    }
  }

  case object Rock extends Subject(1)
  case object Paper extends Subject(2)
  case object Scissors extends Subject(3)

  sealed abstract class Result(val score: Int)

  case object Lost extends Result(0)
  case object Draw extends Result(3)
  case object Won extends Result(6)

  case class Round(opp: Subject, you: Subject)

  def evalRound(round: Round): Result = {
    if (round.you.wins == round.opp) Won
    else if (round.you == round.opp) Draw
    else Lost
  }

  def parseLine(line: String): Option[Round] = Try {
    val items = line.trim.split("\\s+")
    Round(subjectOf(items(0), 'A'), subjectOf(items(1), 'X'))
  }.toOption

  def subjectOf(s: String, base: Char): Subject = s(0) - base match {
    case 0 => Rock
    case 1 => Paper
    case 2 => Scissors
  }

  def nextRound(): Option[Round] =
    Option(StdIn.readLine()) flatMap (parseLine(_) orElse nextRound())
}
