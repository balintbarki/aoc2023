package aoc.aoc2015

import aoc.aoc2015.day1.Day1Puzzle
import aoc.aoc2015.day2.Day2Puzzle
import aoc.aoc2015.day3.Day3Puzzle
import aoc.{DailyPuzzle, PuzzleCollection}

object PuzzleCollection2015 extends PuzzleCollection(2015) {
  override def puzzles: Seq[DailyPuzzle] = Seq(
    Day1Puzzle,
    Day2Puzzle,
    Day3Puzzle
  )
}
