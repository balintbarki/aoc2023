package aoc

abstract class PuzzleCollection(val year: Int) {
  def puzzles: Seq[DailyPuzzle]
}
