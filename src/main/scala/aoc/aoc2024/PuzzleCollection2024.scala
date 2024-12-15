package aoc.aoc2024

import aoc.aoc2024.day1.Day1Puzzle
import aoc.aoc2024.day10.Day10Puzzle
import aoc.aoc2024.day11.Day11Puzzle
import aoc.aoc2024.day2.Day2Puzzle
import aoc.aoc2024.day3.Day3Puzzle
import aoc.aoc2024.day4.Day4Puzzle
import aoc.aoc2024.day5.Day5Puzzle
import aoc.aoc2024.day6.Day6Puzzle
import aoc.aoc2024.day7.Day7Puzzle
import aoc.aoc2024.day8.Day8Puzzle
import aoc.aoc2024.day9.Day9Puzzle
import aoc.{DailyPuzzle, PuzzleCollection}

object PuzzleCollection2024 extends PuzzleCollection(2024) {
  override def puzzles: Seq[DailyPuzzle] = Seq(
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
    Day11Puzzle,
  )
}
