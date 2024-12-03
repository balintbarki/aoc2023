package aoc.aoc2024.day3

import aoc.aoc2024.DailyPuzzle2024

import scala.annotation.tailrec

case object Day3Puzzle extends DailyPuzzle2024(3, "Mull It Over") {

  val EnablePattern = "do()"
  val DisablePattern = "don't()"
  val MultiplyPattern = raw"""mul\((\d{1,3}),(\d{1,3})\)"""
  val LongestMultiply = "mul(123,123)".size

  override def calculatePart1(lines: Seq[String]): String = {

    MultiplyPattern.r.unanchored.findAllIn(lines.mkString).matchData.map { matchData =>
      matchData.group(1).toInt * matchData.group(2).toInt
    }.sum.toString
  }

  override def calculatePart2(lines: Seq[String]): String = {
    processRemainingInput(lines.mkString, true, 0).toString
  }

  @tailrec
  private def processRemainingInput(input: String, enabled: Boolean, accumulator: Int): Int = {
    val startsWithMultiplyPattern = (MultiplyPattern + raw""".*""").r
    val charsToTake = Math.min(input.size, LongestMultiply)
    if (charsToTake > 0) {
      val text = input.take(charsToTake)
      val (charToDrop, newEnabled, valueToAccumulate) = text match {
        case text if text.startsWith(EnablePattern)  =>
          (EnablePattern.size, true, 0)
        case text if text.startsWith(DisablePattern) =>
          (DisablePattern.size, false, 0)
        case startsWithMultiplyPattern(a, b)         =>
          val matchSize = MultiplyPattern.r.findFirstIn(input)
            .getOrElse(throw new IllegalArgumentException(s"Unexpected error")).size
          val valueToAccumulate = if (enabled) a.toInt * b.toInt else 0
          (matchSize, enabled, valueToAccumulate)
        case _                                       => (1, enabled, 0)
      }
      processRemainingInput(input.drop(charToDrop), newEnabled, accumulator + valueToAccumulate)
    }
    else
      accumulator
  }
}
