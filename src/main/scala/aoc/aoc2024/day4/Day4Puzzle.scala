package aoc.aoc2024.day4

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.ImmutableMatrix

case object Day4Puzzle extends DailyPuzzle2024(4, "Ceres Search") {

  val XMAS = "XMAS"
  val xmasPattern = XMAS.r
  val xmasReversePattern = XMAS.reverse.r

  override def calculatePart1(lines: Seq[String]): Long = {
    val transposed = transpose(lines)
    val diagonal1Lines = diagonal(lines)
    val diagonal2Lines = diagonal(rotateLeft(lines))
    Seq(
      findXmas(lines, "Straight"),
      findXmas(transposed, "Transposed"),
      findXmas(diagonal1Lines, "Diagonal1"),
      findXmas(diagonal2Lines, "Diagonal2"),
    ).sum
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val rows = Range(1, lines.size - 1)
    val columns = Range(1, lines.head.size - 1)

    rows.flatMap { row =>
      columns.map { column =>
        if (lines(row)(column) == 'A') {
          val topLeft = lines(row - 1)(column - 1)
          val topRight = lines(row - 1)(column + 1)
          val bottomLeft = lines(row + 1)(column - 1)
          val bottomRight = lines(row + 1)(column + 1)

          val topLeftToBottomRightMatches = ((topLeft == 'M') && (bottomRight == 'S')) || ((topLeft == 'S') && (bottomRight == 'M'))
          val topRightToBottomLeftMatches = ((topRight == 'M') && (bottomLeft == 'S')) || ((topRight == 'S') && (bottomLeft == 'M'))

          if (topLeftToBottomRightMatches && topRightToBottomLeftMatches)
            1
          else
            0
        }
        else
          0
      }
    }.sum
  }

  private def findXmas(lines: Seq[String], dir: String): Int = {
    lines.map { line =>
      val result = xmasPattern.findAllIn(line).size + xmasReversePattern.findAllIn(line).size
      result
    }.sum
  }

  private def diagonal(lines: Seq[String]): Seq[String] = {
    require(lines.forall(_.length == lines.head.length))

    val lineCnt = lines.length + lines.head.length - 1
    val maxItemCnt = Math.min(lines.length, lines.head.length)

    Range.inclusive(0, lineCnt - 1).map(lineId => {
      Range.inclusive(0, maxItemCnt - 1).flatMap(itemId => {
        val row = lineId - itemId
        val column = itemId
        if ((0 <= row) && (row < lines.length) && (0 <= column) && (column <= lines.head.length - 1)) {
          Some(lines(row)(column))
        }
        else
          None
      })
    }.mkString)
  }

  private def transpose(lines: Seq[String]): Seq[String] =
    ImmutableMatrix(lines.map(_.toList).toList).transpose.rows.map(_.mkString)

  private def rotateLeft(lines: Seq[String]): Seq[String] =
    ImmutableMatrix(lines.map(_.toList).toList).rotateLeft.rows.map(_.mkString)

}
