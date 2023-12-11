package aoc.aoc2023

import aoc.DailyPuzzle
import aoc.aoc2023.day1.Day1Puzzle
import aoc.aoc2023.day10.Day10Puzzle
import aoc.aoc2023.day11.Day11Puzzle
import aoc.aoc2023.day2.Day2Puzzle
import aoc.aoc2023.day3.Day3Puzzle
import aoc.aoc2023.day4.Day4Puzzle
import aoc.aoc2023.day5.Day5Puzzle
import aoc.aoc2023.day6.Day6Puzzle
import aoc.aoc2023.day7.Day7Puzzle
import aoc.aoc2023.day8.Day8Puzzle
import aoc.aoc2023.day9.Day9Puzzle
import aoc.utils.FileUtils

import scala.util.{Failure, Success, Try}

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
    Day5Puzzle,
    Day6Puzzle,
    Day7Puzzle,
    Day8Puzzle,
    Day9Puzzle,
    Day10Puzzle,
    Day11Puzzle
  )

  def runPuzzlePart(puzzle: DailyPuzzle, part: Int, inputFileName: String): (String, Long) = {
    val lines = FileUtils.fileToLines(inputFileName)
    val startTime = System.currentTimeMillis()

    val result = Try({
      if (part == 1)
        puzzle.calculatePart1(lines)
      else if (part == 2)
        puzzle.calculatePart2(lines)
      else
        throw new IllegalArgumentException("Unexpected part")
    }) match {
      case Success(value)     => value
      case Failure(exception) => exception.getMessage
    }

    val deltaT = System.currentTimeMillis() - startTime

    (result, deltaT)
  }

  def runPuzzle(day: Int): Unit = {
    val puzzle = puzzles.find(_.day == day).getOrElse(notImplementedPuzzle)
    val inputFileName = puzzle.inputPath

    println(s"Results of Day $day - ${puzzle.name}:")

    val (part1Result, part1Time) = runPuzzlePart(puzzle, 1, inputFileName)
    val (part2Result, part2Time) = runPuzzlePart(puzzle, 2, inputFileName)

    println(f"    Part1: $part1Result%-20s ($part1Time ms)")
    println(f"    Part2: $part2Result%-20s ($part2Time ms)")
  }

  puzzles.map(_.day).sorted.foreach { day => runPuzzle(day) }
}
