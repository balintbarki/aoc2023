package aoc.aoc2023.day21

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImmutableMatrix

import scala.annotation.tailrec
import scala.collection.mutable

case object Day21Puzzle extends DailyPuzzle2023(21, "Step Counter") {

  var stepCnt: Int = 0

  override def calculatePart1(lines: Seq[String]): Long = {
    val input = ImmutableMatrix[Tile](lines.map(line => line.map(c => Tile(c)).toList).toList)

    val coordinateMap = input.getCoordinateMap

    val ((startX, startY), _) = coordinateMap.find { case (_, tile) => tile.isInstanceOf[StartPosition] }
      .getOrElse(???)

    val distanceMap: mutable.Map[(Int, Int), Int] = coordinateMap.map { case ((x, y), _) => ((x, y), Int.MaxValue) }
      .to(mutable.Map)

    @tailrec
    def discoverDistances(modulesToUpdate: List[((Int, Int), Int)], distanceMap: mutable.Map[(Int, Int), Int]): Unit = {

      def checkTileOn(x: Int, y: Int): Boolean = {

        if ((0 <= x) && (x < input.xSize) && (0 <= y) && (y < input.ySize)) {
          val currentDistance = distanceMap.getOrElse((x, y), ???)
          if (input.get(x, y).isInstanceOf[GardenPlot] && currentDistance == Int.MaxValue)
            true
          else
            false
        }
        else
          false
      }

      modulesToUpdate.foreach { case ((x, y), currentDistance) => distanceMap.update((x, y), currentDistance) }

      val nextModules = modulesToUpdate.flatMap { case ((x, y), currentDistance) =>
        List(
          (x - 1, y),
          (x + 1, y),
          (x, y - 1),
          (x, y + 1)
        ).filter { case (x, y) => checkTileOn(x, y) }.distinct.map(key => (key, currentDistance + 1))
      }.distinct

      if (nextModules.nonEmpty)
        discoverDistances(nextModules, distanceMap)
    }

    discoverDistances(List(((startX, startY), 0)), distanceMap)

    distanceMap.count { case (_, distance) => (distance % 2 == stepCnt % 2) && distance <= stepCnt }
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private abstract class Tile

  private class GardenPlot extends Tile

  private class StartPosition extends GardenPlot

  private class Rock extends Tile

  private object Tile {
    def apply(c: Char): Tile = c match {
      case 'S' => new StartPosition
      case '.' => new GardenPlot
      case '#' => new Rock
    }
  }

}
