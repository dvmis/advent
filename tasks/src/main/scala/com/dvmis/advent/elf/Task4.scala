package com.dvmis.advent.elf

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

class Task4 extends Task {
  import Task4.behaviour._

  override def execute(): Unit = println(s"Result = ${doExecute(0)}")

  @tailrec
  private def doExecute(result: Int): Int = nextRound() match {
    case Some(r) => doExecute(result + r.you.score + evalRound(r).score)
    case None => result
  }
}

object Task4 {

  object model {
    import Subject._

    sealed abstract class Subject(val score: Int) {
      final def wins: Subject = winsMap(this)
      final def loses: Subject = losesMap(this)
    }

    object Subject {
      case object Rock extends Subject(1)
      case object Paper extends Subject(2)
      case object Scissors extends Subject(3)

      private val winsMap: Map[Subject, Subject] = Map(Rock -> Scissors, Paper -> Rock, Scissors -> Paper)
      private val losesMap = winsMap map (_.swap)
    }

    sealed abstract class Result(val score: Int)

    object Result {
      case object Lost extends Result(0)
      case object Draw extends Result(3)
      case object Won extends Result(6)
    }

    case class Round(opp: Subject, you: Subject)
  }

  object parser {
    import model._

    private val subjects = Map("A" -> Subject.Rock, "B" -> Subject.Paper, "C" -> Subject.Scissors)
    private val selectors = Map[String, Subject => Subject]("X" -> (_.wins), "Y" -> identity, "Z" -> (_.loses))

    def parseLine(line: String): Option[Round] = Try {
      val items = line.trim split "\\s+"
      val subj = subjects(items(0))
      Round(subj, selectors(items(1))(subj))
    }.toOption
  }

  object behaviour {
    import model._
    import parser._

    def evalRound(round: Round): Result =
      if (round.you.wins == round.opp) Result.Won
      else if (round.you == round.opp) Result.Draw
      else Result.Lost

    def nextRound(): Option[Round] = Option(StdIn.readLine()) flatMap parseLine
  }
}
