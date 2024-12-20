package aoc.aoc2024.day15

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.{Direction, ImmutableMatrix, MutableMatrix}

case object Day15Puzzle extends DailyPuzzle2024(15, "Warehouse Woes") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val (map, steps) = readInput(lines)


    0
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def readInput(lines: Seq[String]): (MutableMatrix[MapElement], Seq[Direction]) = {
    lines.span(_.nonEmpty) match {
      case (mapLines, stepLines) =>
        val map = mapLines.map(line => line.map {
          case '#'        => Wall()
          case 'O'        => Box()
          case '.'        => Empty()
          case '@'        => Robot()
          case unexpected => throw new IllegalArgumentException(s"Unexpected char in input: $unexpected")
        })

        val steps = stepLines.flatMap(line => line.flatMap {
          case '<' => Some(Direction.Left)
          case '>' => Some(Direction.Right)
          case '^' => Some(Direction.Up)
          case 'v' => Some(Direction.Down)
          case _   => None
        })

        (MutableMatrix(map), steps)
    }
  }

  private abstract class MapElement

  private case class Wall() extends MapElement

  private case class Empty() extends MapElement

  private case class Box() extends MapElement

  private case class Robot() extends MapElement

}
