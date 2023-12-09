package aoc.aoc2023

import aoc.DailyPuzzle
import aoc.aoc2023.day1.Day1Puzzle
import aoc.aoc2023.day2.Day2Puzzle
import aoc.aoc2023.day3.Day3Puzzle
import aoc.aoc2023.day4.Day4Puzzle
import aoc.aoc2023.day6.Day6Puzzle
import aoc.aoc2023.day7.Day7Puzzle
import aoc.aoc2023.day8.Day8Puzzle
import aoc.aoc2023.day9.Day9Puzzle
import aoc.utils.FileUtils

object PuzzleRunner extends App {

  private val notImplementedPuzzle: DailyPuzzle = new DailyPuzzle(0, "Not implemented") {
    override def calculatePart1(
      lines: Seq[String]): String = "This puzzle is not implemented"

    override def calculatePart2(
      lines: Seq[String]): String = "This puzzle is not implemented"
  }
  private val puzzles: Seq[DailyPuzzle] = Seq(
    Day1Puzzle,
    Day2Puzzle,
    Day3Puzzle,
    Day4Puzzle,
    Day6Puzzle,
    Day7Puzzle,
    Day8Puzzle,
    Day9Puzzle
  )

  def runPuzzlePart(puzzle: DailyPuzzle, part: Int, inputFileName: String): (String, Long) = {
    val lines = FileUtils.fileToLines(inputFileName)
    val startTime = System.currentTimeMillis()
    val result = if (part == 1)
      puzzle.calculatePart1(lines)
    else if (part == 2)
      puzzle.calculatePart2(lines)
    else
      throw new IllegalArgumentException("Unexpected part")
    val deltaT = System.currentTimeMillis() - startTime

    (result, deltaT)
  }

  def runPuzzle(day: Int): Unit = {
    val puzzle = puzzles.find(_.day == day).getOrElse(notImplementedPuzzle)
    val inputFileName = puzzle.inputPath

    val (part1Result, part1Time) = runPuzzlePart(puzzle, 1, inputFileName)
    val (part2Result, part2Time) = runPuzzlePart(puzzle, 2, inputFileName)

    println(s"Results of Day $day - ${puzzle.name}:")
    println(s"    Part1: $part1Result ($part1Time ms)")
    println(s"    Part2: $part2Result ($part2Time ms)")
  }

  puzzles.map(_.day).sorted.foreach { day => runPuzzle(day) }
}
