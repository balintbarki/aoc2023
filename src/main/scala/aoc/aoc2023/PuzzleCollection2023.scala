package aoc.aoc2023

import aoc.{DailyPuzzle, PuzzleCollection}
import aoc.aoc2023.day1.Day1Puzzle
import aoc.aoc2023.day10.Day10Puzzle
import aoc.aoc2023.day11.Day11Puzzle
import aoc.aoc2023.day12.Day12Puzzle
import aoc.aoc2023.day13.Day13Puzzle
import aoc.aoc2023.day14.Day14Puzzle
import aoc.aoc2023.day15.Day15Puzzle
import aoc.aoc2023.day16.Day16Puzzle
import aoc.aoc2023.day18.Day18Puzzle
import aoc.aoc2023.day19.Day19Puzzle
import aoc.aoc2023.day2.Day2Puzzle
import aoc.aoc2023.day20.Day20Puzzle
import aoc.aoc2023.day21.Day21Puzzle
import aoc.aoc2023.day3.Day3Puzzle
import aoc.aoc2023.day4.Day4Puzzle
import aoc.aoc2023.day5.Day5Puzzle
import aoc.aoc2023.day6.Day6Puzzle
import aoc.aoc2023.day7.Day7Puzzle
import aoc.aoc2023.day8.Day8Puzzle
import aoc.aoc2023.day9.Day9Puzzle

object PuzzleCollection2023 extends PuzzleCollection(2023) {
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
    Day12Puzzle,
    Day13Puzzle,
    Day14Puzzle,
    Day15Puzzle,
    Day16Puzzle,
    Day18Puzzle,
    Day19Puzzle,
    Day20Puzzle,
    Day21Puzzle
  )
}
