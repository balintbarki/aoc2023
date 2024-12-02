package aoc.aoc2024.day2

import aoc.aoc2024.DailyPuzzle2024

case object Day2Puzzle extends DailyPuzzle2024(2, "Red-Nosed Reports") {
  override def calculatePart1(lines: Seq[String]): String = {
    val inputs = getInputs(lines)
    inputs.count(isSafe).toString
  }

  override def calculatePart2(lines: Seq[String]): String = {
    val inputs = getInputs(lines)

    inputs.count { input =>
      Range.inclusive(0, input.size - 1).exists { indexToOmit =>
        isSafe(input.zipWithIndex.filter(_._2 != indexToOmit).map(_._1))
      }

    }.toString
  }

  private def getInputs(lines: Seq[String]): Seq[Seq[Int]] = lines.map {
    _.split("\\s+").map(_.toInt)
  }

  private def isSafe(levels: Seq[Int]): Boolean = {
    val sortedLevels = levels.sorted
    val isSorted = ((sortedLevels == levels) || (sortedLevels == levels.reverse))
    val isDistinct = levels.distinct.size == levels.size
    val isChangeWithinLimits = levels.sliding(2).forall { pair =>
      val diff = (pair.head - pair(1)).abs
      (1 <= diff) && (diff <= 3)
    }
    isSorted && isDistinct && isChangeWithinLimits
  }

}
