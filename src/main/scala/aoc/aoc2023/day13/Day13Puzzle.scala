package aoc.aoc2023.day13

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImplicitUtils._
import aoc.utils.ImmutableMatrix

case object Day13Puzzle extends DailyPuzzle2023(13, "Point of Incidence") {

  override def calculatePart1(lines: Seq[String]): Long = {
    calculate(lines, 0)
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    calculate(lines, 1)
  }

  private def calculate(lines: Seq[String], allowedDiff: Int): Long = {
    val inputs = lines.toList.multiSpanWithoutDelimiter(_.isEmpty)
    inputs.map { input =>
      val matrix = ImmutableMatrix.fromStrings(input)
      val rowMirrorLineOpt = (1 until matrix.rows.length)
        .find(row => (matrix ~= matrix.mirrorAtRow(row)).sumAll == allowedDiff * 2)
        .getOrElse(0)
      val columnMirrorLineOpt = (1 until matrix.columns.length)
        .find(column => (matrix ~= matrix.mirrorAtColumn(column)).sumAll == allowedDiff * 2)
        .getOrElse(0)

      val result = rowMirrorLineOpt * 100 + columnMirrorLineOpt
      if (result == 0) {
        input.foreach(println)
        throw new RuntimeException("Result should never be 0")
      }
      result
    }.sum
  }
}
