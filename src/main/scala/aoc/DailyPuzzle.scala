package aoc

import aoc.utils.FileUtils

abstract class DailyPuzzle(val day: Int, val name: String) {

  val inputPath: String = FileUtils.getPuzzleInputPath(day)

  def calculatePart1(lines: Seq[String]): String

  def calculatePart2(lines: Seq[String]): String
}
