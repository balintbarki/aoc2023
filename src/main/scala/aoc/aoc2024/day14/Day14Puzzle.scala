package aoc.aoc2024.day14

import aoc.aoc2024.DailyPuzzle2024

import scala.collection.mutable

case object Day14Puzzle extends DailyPuzzle2024(14, "Restroom Redoubt") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val (xSize, ySize, inputs) = getInput(lines)
    val time = 100
    val newPositions = inputs.map { case (x, y, vx, vy) =>
      calculateNewPosition(x, y, vx, vy, xSize, ySize, time)
    }

    val firstQuarterCnt = calcQuadrant(newPositions, xSize, ySize, 1)
    val secondQuarterCnt = calcQuadrant(newPositions, xSize, ySize, 2)
    val thirdQuarterCnt = calcQuadrant(newPositions, xSize, ySize, 3)
    val fourthQuarterCnt = calcQuadrant(newPositions, xSize, ySize, 4)

    firstQuarterCnt * secondQuarterCnt * thirdQuarterCnt * fourthQuarterCnt
  }


  override def calculatePart2(lines: Seq[String]): Long = {
    val (xSize, ySize, inputs) = getInput(lines)

    var smallestVariance: Double = 100000
    var smallestVarianceTime = 0

    (1 to 100000).foreach { i =>
      val newPositions = inputs.map { case (x, y, vx, vy) =>
        calculateNewPosition(x, y, vx, vy, xSize, ySize, i)
      }

      val variance = {
        val avgX = newPositions.map(_._1).sum / newPositions.size
        val avgY = newPositions.map(_._2).sum / newPositions.size
        newPositions.map { case (x, y, _, _) =>
          math.pow(avgX - x, 2) + math.pow(avgY - x, 2)
        }.sum / newPositions.size
      }

      if (variance < smallestVariance) {
        smallestVariance = variance
        smallestVarianceTime = i
        println(s"$i: ")
        printPositions(newPositions, xSize, ySize)
        println()
      }
    }

    smallestVarianceTime
  }

  private def calcQuadrant(positions: Seq[(Int, Int, Int, Int)], xSize: Int, ySize: Int, quadrant: Int) = {
    val middleX = (xSize - 1) / 2
    val middleY = (ySize - 1) / 2
    quadrant match {
      case 1 => positions.count { case (x, y, _, _) => (x < middleX) && (y < middleY) }
      case 2 => positions.count { case (x, y, _, _) => (x > middleX) && (y < middleY) }
      case 3 => positions.count { case (x, y, _, _) => (x < middleX) && (y > middleY) }
      case 4 => positions.count { case (x, y, _, _) => (x > middleX) && (y > middleY) }
    }

  }

  private def calculateNewPosition(x: Int, y: Int, vx: Int, vy: Int, xSize: Int, ySize: Int, time: Int) = {
    val newX = (x + vx * time) % xSize
    val newY = (y + vy * time) % ySize
    (if (newX < 0) newX + xSize else newX, if (newY < 0) newY + ySize else newY, vx, vy)
  }

  private def mightBeChristmasTree(positions: Seq[(Int, Int, Int, Int)], xSize: Int, ySize: Int): Boolean = {
    val xMin = positions.map(_._1).min
    val xMax = positions.map(_._1).max
    val yMin = positions.map(_._2).min
    val yMax = positions.map(_._2).max

    val firstQuarterCnt = calcQuadrant(positions, xSize, ySize, 1)
    val secondQuarterCnt = calcQuadrant(positions, xSize, ySize, 2)
    val thirdQuarterCnt = calcQuadrant(positions, xSize, ySize, 3)
    val fourthQuarterCnt = calcQuadrant(positions, xSize, ySize, 4)

    val symmetricQuarters = (firstQuarterCnt == secondQuarterCnt) && (thirdQuarterCnt == fourthQuarterCnt)

    val variance = {
      val avgX = positions.map(_._1).sum / positions.size
      val avgY = positions.map(_._2).sum / positions.size
      positions.map { case (x, y, _, _) =>
        math.pow(avgX - x, 2) + math.pow(avgY - x, 2)
      }.sum / positions.size
    }

    symmetricQuarters || (variance < 1000)
  }

  private def printPositions(positions: Seq[(Int, Int, Int, Int)], xSize: Int, ySize: Int): Unit = {

    val content: mutable.Seq[mutable.Seq[Char]] = mutable.Seq.fill(ySize, xSize)('.')
    positions.foreach { case (x, y, _, _) => content(y).update(x, '#') }
    content.foreach(line => println(line.mkString))
    println()
    println()
  }

  private def getInput(lines: Seq[String]): (Int, Int, Seq[(Int, Int, Int, Int)]) = {
    val pattern = """p=(\d+),(\d+) v=(.+),(.+)""".r

    val inputs = lines.map {
      case pattern(x, y, vx, vy) => (x.toInt, y.toInt, vx.toInt, vy.toInt)
    }
    val xMax = inputs.map(_._1).max + 1
    val yMax = inputs.map(_._2).max + 1

    (xMax, yMax, inputs)
  }

}
