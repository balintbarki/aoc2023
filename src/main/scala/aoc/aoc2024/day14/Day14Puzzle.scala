package aoc.aoc2024.day14

import aoc.aoc2024.DailyPuzzle2024

case object Day14Puzzle extends DailyPuzzle2024(14, "Restroom Redoubt") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val (xSize, ySize, inputs) = getInput(lines)
    val time = 100
    val newPositions = inputs.map { case (x, y, vx, vy) =>
      val newX = (x + vx * time) % xSize
      val newY = (y + vy * time) % ySize
      (if (newX < 0) newX + xSize else newX, if (newY < 0) newY + ySize else newY)
    }
    val middleX = (xSize - 1) / 2
    val middleY = (ySize - 1) / 2

    val firstQuarterCnt = newPositions.count { case (x, y) => (x < middleX) && (y < middleY) }
    val secondQuarterCnt = newPositions.count { case (x, y) => (x > middleX) && (y < middleY) }
    val thirdQuarterCnt = newPositions.count { case (x, y) => (x < middleX) && (y > middleY) }
    val fourthQuarterCnt = newPositions.count { case (x, y) => (x > middleX) && (y > middleY) }

    firstQuarterCnt * secondQuarterCnt * thirdQuarterCnt * fourthQuarterCnt
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def getInput(lines: Seq[String]): (Int, Int, Seq[(Int, Int, Int, Int)]) = {
    val pattern = """p=(\d+),(\d+) v=(.+),(.+)""".r

    val inputs = lines.map {
      case pattern(x, y, vx, vy) => (x.toInt, y.toInt, vx.toInt, vy.toInt)
    }
    val xSize = inputs.map(_._1).max + 1
    val ySize = inputs.map(_._2).max + 1

    (xSize, ySize, inputs)
  }

}
