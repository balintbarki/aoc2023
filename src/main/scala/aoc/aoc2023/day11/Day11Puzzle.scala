package aoc.aoc2023.day11

import aoc.aoc2023.DailyPuzzle2023

import scala.math.abs

case object Day11Puzzle extends DailyPuzzle2023(11, "Cosmic Expansion") {

  override def calculatePart1(
    lines: Seq[String]): String = calculateDistanceSumInExpandedUniverse(lines, 2)

  override def calculatePart2(
    lines: Seq[String]): String = calculateDistanceSumInExpandedUniverse(lines, 1000000)

  private def calculateDistanceSumInExpandedUniverse(lines: Seq[String], times: Long) = {

    val expandedGalaxies = expandUniverse(lines, times)

    {
      for {
        (first, firstIdx) <- expandedGalaxies.zipWithIndex
        (second, secondIdx) <- expandedGalaxies.zipWithIndex
        if firstIdx < secondIdx
      } yield abs(first._1 - second._1) + abs(first._2 - second._2)
    }.sum.toString

  }

  private def expandUniverse(lines: Seq[String], times: Long): Seq[(Long, Long)] = {

    val columnIndices = lines.head.indices
    val rowIndices = lines.indices

    val rowsToExpand = rowIndices.filter(lines(_).forall(_ == '.'))
    val columnsToExpand = columnIndices.filter(colIdx => columnIndices.forall(rowIdx => lines(rowIdx)(colIdx) == '.'))

    val galaxies = rowIndices
      .flatMap(
        rowIdx => columnIndices.flatMap(colIdx => if (lines(rowIdx)(colIdx) == '#') Some((colIdx, rowIdx)) else None))

    val expandedGalaxies = galaxies
      .map { case (colIdx, rowIdx) =>
        val newColIdx = colIdx + columnsToExpand.count(_ < colIdx) * (times - 1)
        val newRowIdx = rowIdx + rowsToExpand.count(_ < rowIdx) * (times - 1)

        (newColIdx, newRowIdx)
      }

    expandedGalaxies
  }
}
