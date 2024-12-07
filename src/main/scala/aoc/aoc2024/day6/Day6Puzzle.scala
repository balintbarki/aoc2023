package aoc.aoc2024.day6

import aoc.aoc2024.DailyPuzzle2024
import aoc.aoc2024.day6.Day6Puzzle.Tile
import aoc.utils.{Direction, Matrix}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

case object Day6Puzzle extends DailyPuzzle2024(6, "Guard Gallivant") {

  import TileMatrixExtension._

  override def calculatePart1(lines: Seq[String]): Long = {
    val (map, (startX, startY)) = loadMapAndStartCoordinates(lines)

    walkMap(map, startX, startY)

    map.count {
      case walkable: Walkable => walkable.visited
      case _                  => false
    }
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val (map, (startX, startY)) = loadMapAndStartCoordinates(lines)

    val (walkedTiles, _, _) = walkMap(map.copy, startX, startY)

    walkedTiles.drop(1).distinct.count { case (x, y) =>
      if (x != startX || y != startY) {
        val mapWithObstacle = map.copy.updated(x, y, new ExtraObstacle())
        val (_, _, loopFound) = walkMap(mapWithObstacle, startX, startY)
        if (loopFound)
          printMap(mapWithObstacle)
        loopFound
      }
      else
        false
    }
  }

  private def printMap(map: Matrix[Tile]): Unit = {
    map.elements.map(line => line.map {
      case tile: Walkable if tile.visited => 'O'
      case _: Walkable                    => '.'
      case _: ExtraObstacle               => 'X'
      case _: Obstacle                    => '#'
    }.mkString).foreach(println)
    println()
  }

  private def walkMap(map: Matrix[Tile], startX: Int, startY: Int): (Seq[(Int, Int)], Boolean, Boolean) = {

    val cacheFindTiles: mutable.Map[(Int, Int, Direction), (Seq[(Int, Int)], Boolean)] = mutable.Map.empty

    def findTilesBeforeNextObstacleFrom(x: Int, y: Int, dir: Direction): (Seq[(Int, Int)], Boolean, Boolean) = {

      cacheFindTiles.get((x, y, dir)) match {
        case Some((coordinates, mapEndFound)) => (coordinates, mapEndFound, true)
        case None                             =>
          val tilesToMapEndInWalkOrder = dir match {
            case Direction.Up    => Range(0, y).reverse.map((x, _))
            case Direction.Down  => Range(y + 1, map.ySize).map((x, _))
            case Direction.Left  => Range(0, x).reverse.map((_, y))
            case Direction.Right => Range(x + 1, map.xSize).map((_, y))
          }

          val tilesToObstacleInWalkOrder = tilesToMapEndInWalkOrder
            .takeWhile { case (_x, _y) => !map.get(_x, _y).isInstanceOf[Obstacle] }

          val mapEndReached = tilesToObstacleInWalkOrder.size == tilesToMapEndInWalkOrder.size

          cacheFindTiles.addOne((x, y, dir), (tilesToObstacleInWalkOrder, mapEndReached))
          (tilesToObstacleInWalkOrder, mapEndReached, false)
      }
    }

    @tailrec
    def doWalkMap(
      x: Int, y: Int, dir: Direction, alreadyWalkedTiles: Seq[(Int, Int)]): (Seq[(Int, Int)], Boolean, Boolean) = {

      val (tilesBeforeNextObstackle, mapEndReached, loopFound) = findTilesBeforeNextObstacleFrom(x, y, dir)
      tilesBeforeNextObstackle.foreach { case (_x, _y) => map.get(_x, _y).asInstanceOf[Walkable].visit() }

      val newlyWalkedTiles = alreadyWalkedTiles ++ tilesBeforeNextObstackle

      if (mapEndReached || loopFound) {
        (newlyWalkedTiles, mapEndReached, loopFound)
      }
      else {
        val (nextX, nextY) = newlyWalkedTiles.last
        doWalkMap(nextX, nextY, dir.rotateRight, newlyWalkedTiles)
      }
    }

    doWalkMap(startX, startY, Direction.Up, Seq((startX, startY)))
  }

  private def loadMapAndStartCoordinates(lines: Seq[String]): (Matrix[Tile], (Int, Int)) = {
    var (startX, startY) = (-1, -1)
    val tiles = lines.indices.map(lineIdx => lines(lineIdx).indices.map(colIdx => lines(lineIdx)(colIdx) match {
      case '.' => new Walkable()
      case '#' => new Obstacle()
      case '^' => startX = colIdx; startY = lineIdx; new Start()
      case c   => throw new IllegalArgumentException(s"Unexpected character in input: $c")
    }))

    (Matrix(tiles), (startX, startY))
  }

  abstract class Tile() extends Cloneable {

    def copy: Tile
  }

  class Obstacle() extends Tile {
    def copy: Tile = new Obstacle()
  }

  class ExtraObstacle() extends Obstacle {
    override def copy: Tile = new ExtraObstacle()
  }

  class Walkable() extends Tile {
    var visited = false

    def visit(): Unit = visited = true

    override def copy: Tile = {
      val newWalkable = new Walkable()
      if (visited) newWalkable.visit()
      newWalkable
    }
  }

  class Start() extends Walkable {
    visit()

    override def copy: Tile = new Start()
  }


}

object TileMatrixExtension {

  implicit class MatrixCopy(matrix: Matrix[Tile]) {
    def copy: Matrix[Tile] = {
      val rows = matrix.rows
      Matrix(rows.map(row => row.map(element => element.copy)))
    }
  }
}
