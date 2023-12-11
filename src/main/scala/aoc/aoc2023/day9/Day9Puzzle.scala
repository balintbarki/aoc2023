package aoc.aoc2023.day9

import aoc.aoc2023.DailyPuzzle2023

case object Day9Puzzle extends DailyPuzzle2023(9, "Mirage Maintenance") {
  override def calculatePart1(
    lines: Seq[String]): String = getInput(lines).map(extrapolateLast).sum.toString

  override def calculatePart2(
    lines: Seq[String]): String = getInput(lines).map(extrapolateFirst).sum.toString

  private def getInput(lines: Seq[String]): Seq[Seq[Long]] = {
    val numberRegex = """(-?\d+)""".r

    lines.map { line => {
      numberRegex.findAllIn(line).matchData.map(_.matched.toLong).toSeq
    }
    }
  }

  private def extrapolateLast(input: Seq[Long]): Long = {

    var step = ExtrapolationStep(Seq(input.head), input.tail)

    do {
      val newStep = ExtrapolationStep(step.initials :+ step.diffs.head,
        step.diffs.sliding(2).map { case Seq(first, second) => second - first }.toSeq)
      step = newStep
    } while (!step.diffs.forall(_ == 0))

    step = step.copy(diffs = step.diffs :+ 0)

    while (step.initials.size > 1) {
      val newInitials = step.initials.dropRight(1)
      val newDiffs = step.diffs.foldLeft(Seq(step.initials.last))((numbers, diff) => numbers :+ (numbers.last + diff))
      val newStep = ExtrapolationStep(newInitials, newDiffs)
      step = newStep
    }

    step.diffs.last
  }

  private def extrapolateFirst(input: Seq[Long]): Long = {
    extrapolateLast(input.reverse)
  }

  private case class ExtrapolationStep(initials: Seq[Long], diffs: Seq[Long])
}
