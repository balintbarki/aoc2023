package aoc

import aoc.utils.FileUtils

import java.nio.file.Path

abstract class DailyPuzzle(val year: Int, val day: Int, val name: String) {

  val inputPath: Path = FileUtils.getPuzzleInputPath(year, day)

  def calculatePart1(lines: Seq[String]): Long

  def calculatePart2(lines: Seq[String]): Long
}
