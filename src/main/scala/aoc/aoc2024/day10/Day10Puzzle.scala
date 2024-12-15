package aoc.aoc2024.day10

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.Matrix

import scala.collection.mutable

case object Day10Puzzle extends DailyPuzzle2024(10, "Hoof It") {


  override def calculatePart1(lines: Seq[String]): Long = {
    val map = Matrix.fromStringsToInts(lines)
    val xMax = map.xSize - 1
    val yMax = map.ySize - 1

    val coordinateMap = map.getCoordinateMap
    val trailHeads = coordinateMap.filter { case ((_, _), value) => value == 0 }

    val trailCntCache: mutable.Map[(Int, Int), Seq[(Int, Int)]] = mutable.Map.empty

    def checkAndGetTrailTopsReachableFrom(x: Int, y: Int, current: Int): Seq[(Int, Int)] = {
      if ((0 <= x) && (x <= xMax) && (0 <= y) && (y <= yMax) && (coordinateMap((x, y)) == current + 1))
        getTrailTopsReachableFrom((x, y))
      else
        Seq.empty
    }

    def getTrailTopsReachableFrom(from: (Int, Int)): Seq[(Int, Int)] = {
      val currentPoint = coordinateMap(from)

      if (currentPoint == 9)
        Seq(from)
      else
        trailCntCache.getOrElseUpdate(from, {
          val (x, y) = from

          val left = checkAndGetTrailTopsReachableFrom(x - 1, y, currentPoint)
          val right = checkAndGetTrailTopsReachableFrom(x + 1, y, currentPoint)
          val up = checkAndGetTrailTopsReachableFrom(x, y - 1, currentPoint)
          val down = checkAndGetTrailTopsReachableFrom(x, y + 1, currentPoint)

          left ++ right ++ up ++ down
        })
    }

    trailHeads.map { case ((x, y), _) => getTrailTopsReachableFrom((x, y)).distinct }.flatten.size
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val map = Matrix.fromStringsToInts(lines)

    val xMax = map.xSize - 1
    val yMax = map.ySize - 1

    val coordinateMap = map.getCoordinateMap
    val trailHeads = coordinateMap.filter { case ((_, _), value) => value == 0 }

    val trailRatingCache: mutable.Map[(Int, Int), Int] = mutable.Map.empty

    def checkAndGetTrailRatingFrom(x: Int, y: Int, current: Int): Int = {
      if ((0 <= x) && (x <= xMax) && (0 <= y) && (y <= yMax) && (coordinateMap((x, y)) == current + 1))
        getTrailRatingFrom((x, y))
      else
        0
    }

    def getTrailRatingFrom(from: (Int, Int)): Int = {
      val currentPoint = coordinateMap(from)

      if (currentPoint == 9)
        1
      else
        trailRatingCache.getOrElseUpdate(from, {
          val (x, y) = from

          val left = checkAndGetTrailRatingFrom(x - 1, y, currentPoint)
          val right = checkAndGetTrailRatingFrom(x + 1, y, currentPoint)
          val up = checkAndGetTrailRatingFrom(x, y - 1, currentPoint)
          val down = checkAndGetTrailRatingFrom(x, y + 1, currentPoint)

          left + right + up + down
        })
    }

    trailHeads.map { case ((x, y), _) => getTrailRatingFrom((x, y)) }.sum
  }
}
