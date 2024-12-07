package aoc.aoc2023.day18

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.Direction
import aoc.utils.geometry.{Point, Polygon}

case object Day18Puzzle extends DailyPuzzle2023(18, "unknown") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val inputs = lines
      .map(_.split(" ").toSeq)
      .map { case Seq(dir: String, length: String, _) => (Direction.parse(dir), length.toLong) }

    calculate(inputs)
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val codeRegex = """\(#([0-9a-f]{5})([0-3])\)""".r

    val inputs = lines
      .map(_.split(" ").toSeq)
      .map { case Seq(_, _, code: String) => code match {
        case codeRegex(step, dir) => (dir match {
          case "0" => Direction.Right
          case "1" => Direction.Down
          case "2" => Direction.Left
          case "3" => Direction.Up
        }, java.lang.Long.parseLong(step, 16))
      }
      }

    calculate(inputs)
  }

  private def calculate(inputs: Seq[(Direction, Long)]): Long = {
    val points = inputs.foldLeft(List(Point(0L, 0L))) { case (acc, (dir, step)) =>
      val (dx, dy) = dir match {
        case Direction.Up    => (0L, step)
        case Direction.Down  => (0L, -step)
        case Direction.Left  => (-step, 0L)
        case Direction.Right => (step, 0L)
      }
      acc :+ Point(acc.last.x + dx, acc.last.y + dy)
    }

    val polygon = Polygon(points.drop(1))

    (polygon.area + polygon.perimeter / 2 + 1).toLong
  }

}
