package aoc.aoc2024

import aoc.aoc2024.day1.Day1Puzzle
import aoc.aoc2024.day2.Day2Puzzle
import aoc.{DailyPuzzle, PuzzleCollection}

object PuzzleCollection2024 extends PuzzleCollection(2024) {
  override def puzzles: Seq[DailyPuzzle] = Seq(
    Day1Puzzle,
    Day2Puzzle,
  )
}
