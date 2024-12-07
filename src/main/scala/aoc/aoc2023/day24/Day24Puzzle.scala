package aoc.aoc2023.day24

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.Parsing
import aoc.utils.geometry.Vector

import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}

case object Day24Puzzle extends DailyPuzzle2023(24, "unknown") {

  var minPos: BigDecimal = BigDecimal(7)
  var maxPos: BigDecimal = BigDecimal(27)

  override def calculatePart1(lines: Seq[String]): Long = {

    val input = parseInput(lines)

    val intersections = input.combinations(2).toSeq.map
    { case Seq((position1, velocity1), (position2, velocity2)) => getXYIntersection(position1, velocity1, position2,
      velocity2)
    }

    intersections.count {
      case (Some(vector), Some(t1), Some(t2)) => (0 <= t1) && (0 <= t2) && (minPos <= vector.x) && (vector
        .x < maxPos) && (minPos <= vector.y) && (vector.y < maxPos)
      case _                                  => false
    }
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def parseInput(lines: Seq[String]): Seq[(Vector, Vector)] = {
    lines.map(
      line => line.split("@").toSeq.map(linePart => Parsing.stringToNumbers(linePart) match {
        case Seq(x, y, z) => Vector(x, y, z)
      }) match {
        case Seq(position, velocity) => (position, velocity)
      })
  }

  private def getXYIntersection(
    position1: Vector, velocity1: Vector, position2: Vector,
    velocity2: Vector): (Option[Vector], Option[BigDecimal], Option[BigDecimal]) = {

    val scale = 8
    val roundingMode = RoundingMode.HALF_UP

    val x1 = position1.x
    val y1 = position1.y
    val z1 = position1.z
    val vx1 = velocity1.x
    val vy1 = velocity1.y
    val vz1 = velocity1.z
    val x2 = position2.x
    val y2 = position2.y
    val z2 = position2.z
    val vx2 = velocity2.x
    val vy2 = velocity2.y
    val vz2 = velocity2.z

    Try({
      val t1Tmp = ((x1 - x2) / vx2 - (y1 - y2) / vy2) / (vy1 / vy2 - vx1 / vx2)
      val t2Tmp = (y1 - y2) / vy2 + vy1 / vy2 * t1Tmp
      (t1Tmp, t2Tmp)
    }) match {
      case Success((t1, t2)) =>
        val vector1 = Vector(x1 + vx1 * t1, y1 + vy1 * t1, z1 + vz1 * t1)
        val vector2 = Vector(x2 + vx2 * t2, y2 + vy2 * t2, z2 + vz2 * t2)

        require(vector1.x.setScale(scale, roundingMode) == vector2.x.setScale(scale, roundingMode),
          s"X coordinate mismatch: ${vector1.x}, ${vector2.x}")
        require(vector1.y.setScale(scale, roundingMode) == vector2.y.setScale(scale, roundingMode),
          s"Y coordinate mismatch: ${vector1.y}, ${vector2.y}")

        (Some(vector1), Some(t1), Some(t2))

      case Failure(_) => (None, None, None)
    }
  }
}
