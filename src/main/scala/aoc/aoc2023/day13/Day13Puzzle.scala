package aoc.aoc2023.day13

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImplicitUtils._
import aoc.utils.Matrix

case object Day13Puzzle extends DailyPuzzle2023(13, "Point of Incidence") {

  override def calculatePart1(lines: Seq[String]): String = {

    val inputs = lines.toList.multiSpanWithoutDelimiter(_.isEmpty)

    inputs.map { input =>
      val matrix = Matrix.fromStrings(input)
      val rowHashes = matrix.rowHashes.zipWithIndex
      val columnHashes = matrix.columnHashes.zipWithIndex
      val rowReflectionIndex = findReflection(rowHashes)
      val columnReflectionIndex = findReflection(columnHashes)
      val result = columnReflectionIndex + 100 * rowReflectionIndex
      if (result == 0) {
        input.foreach(println)
        throw new RuntimeException("Result should never be 0")
      }
      result
    }.sum.toString
  }

  override def calculatePart2(lines: Seq[String]): String = {
    val inputs = lines.toList.multiSpanWithoutDelimiter(_.isEmpty)
    inputs.map { input =>
      val matrix = Matrix.fromStrings(input)
      val rowMirrorLine = (1 until matrix.rows.length).find(row => (matrix ~= matrix.mirrorAtRow(row)).sumAll == 1)
        .getOrElse(throw new IllegalArgumentException("Row mirror line should have been found"))
      val columnMirrorLine = (1 until matrix.columns.length)
        .find(column => (matrix ~= matrix.mirrorAtColumn(column)).sumAll == 1)
        .getOrElse(throw new IllegalArgumentException("Row mirror line should have been found"))
      rowMirrorLine * 100 + columnMirrorLine
    }.sum.toString
  }


  private def findReflection(inputs: List[(Int, Int)]): Int = {
    val inputHashes = inputs.map { case (hash, _) => hash }
    val mirrorLineOpt = (1 until inputHashes.length).find(index => {

      val takeCnt = math.min(index, inputs.length - index)
      val (first, second) = inputHashes.splitAt(index)
      first.takeRight(takeCnt) == second.take(takeCnt).reverse
    })

    mirrorLineOpt match {
      case Some(mirrorLine) => mirrorLine
      case _                => 0
    }
  }
}
