package aoc.aoc2024.day6

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.{Direction, Matrix}

import scala.annotation.tailrec

case object Day6Puzzle extends DailyPuzzle2024(6, "Guard Gallivant") {
  override def calculatePart1(lines: Seq[String]): Long = {
    val map = loadMap(lines)

    walkMap(map)

    map.count {
      case walkable: Walkable => walkable.visited
      case _                  => false
    }
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def walkMap(map: Matrix[Tile]): Unit = {

    @tailrec
    def doWalkMap(x: Int, y: Int, dir: Direction): Unit = {
      val tile = map.get(x, y)

      tile match {
        case walkable: Walkable => walkable.visit
        case _                  => throw new IllegalArgumentException(s"Unexpected tile")
      }

      val (maybeNextX, maybeNextY) = dir match {
        case Direction.Up    => (x, y - 1)
        case Direction.Down  => (x, y + 1)
        case Direction.Left  => (x - 1, y)
        case Direction.Right => (x + 1, y)
      }

      if (maybeNextX < 0 || maybeNextY < 0 || map.xSize <= maybeNextX || map.ySize <= maybeNextY) {
        // We are out of the map
      } else {
        val maybeNextTile = map.get(maybeNextX, maybeNextY)

        val (nextX, nextY, nextDir) = maybeNextTile match {
          case _: Walkable => (maybeNextX, maybeNextY, dir)
          case _: Obstacle => (x, y, dir.rotateRight)
        }

        doWalkMap(nextX, nextY, nextDir)
      }
    }

    val coordinateMap = map.getCoordinateMap
    val ((x, y), _) = coordinateMap.find { case (_, tile) => tile.isInstanceOf[Start] }
      .getOrElse(throw new IllegalArgumentException(s"Start position not found"))

    doWalkMap(x, y, Direction.Up)
  }

  private def loadMap(lines: Seq[String]): Matrix[Tile] = {
    Matrix(lines.map(line => line.map {
      case '.' => new Walkable()
      case '#' => new Obstacle()
      case '^' => new Start()
      case c   => throw new IllegalArgumentException(s"Unexpected character in input: $c")
    }))
  }


  private abstract class Tile()

  private class Obstacle extends Tile {
  }

  private class Walkable extends Tile {
    var visited = false

    def visit: Unit = visited = true
  }

  private class Start extends Walkable {
    this.visit
  }

}
