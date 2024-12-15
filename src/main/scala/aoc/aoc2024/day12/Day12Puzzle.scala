package aoc.aoc2024.day12

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.Matrix

case object Day12Puzzle extends DailyPuzzle2024(12, "Garden Groups") {

  val UNKNOWN_REGION = 0

  override def calculatePart1(lines: Seq[String]): Long = {

    val garden = readGarden(lines)
    val plotCoordinateMap = garden.getCoordinateMap

    def findPlotWithNoRegion: Option[((Int, Int), Plot)] = plotCoordinateMap.find(_._2.regionId == UNKNOWN_REGION)

    var plotWithNoRegionOpt = findPlotWithNoRegion
    var regionCnt = 0

    while (plotWithNoRegionOpt.nonEmpty) {

      def discoverRegion(x: Int, y: Int, regionId: Int, plantId: Char): Unit = {
        if ((0 <= x) && (x < garden.xSize) && (0 <= y) && (y < garden.ySize)) {
          val plot = plotCoordinateMap((x, y))
          if ((plot.regionId == 0) && (plot.plantId == plantId)) {
            plot.regionId = regionId
            discoverRegion(x - 1, y, regionId, plantId)
            discoverRegion(x + 1, y, regionId, plantId)
            discoverRegion(x, y - 1, regionId, plantId)
            discoverRegion(x, y + 1, regionId, plantId)
          }
        }
      }

      val ((x, y), plot) = plotWithNoRegionOpt.get

      regionCnt += 1

      discoverRegion(x, y, regionCnt, plot.plantId)

      plotWithNoRegionOpt = findPlotWithNoRegion
    }

    setUpFences(plotCoordinateMap, garden.xSize - 1, garden.ySize - 1)

    val allPlots = garden.allElements
    allPlots.map(_.regionId).distinct.map { regionId =>
      val plotsInRegion = allPlots.filter(_.regionId == regionId)
      plotsInRegion.map(_.fenceCnt).sum * plotsInRegion.size
    }.sum
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def setUpFences(plotCoordinateMap: Map[(Int, Int), Plot], maxX: Int, maxY: Int): Unit = {
    plotCoordinateMap.foreach { case ((x, y), plot) =>
      if ((x == 0) || (plotCoordinateMap((x - 1, y)).regionId != plot.regionId))
        plot.leftFence = true

      if ((y == 0) || (plotCoordinateMap((x, y - 1)).regionId != plot.regionId))
        plot.upFence = true

      if ((x == maxX) || (plotCoordinateMap((x + 1, y)).regionId != plot.regionId))
        plot.rightFence = true

      if ((y == maxY) || (plotCoordinateMap((x, y + 1)).regionId != plot.regionId))
        plot.downFence = true
    }
  }

  private def readGarden(lines: Seq[String]) =
    Matrix(lines.map(line => line.map { c => new Plot(c) }))


  private class Plot(val plantId: Char) {
    var regionId: Int = UNKNOWN_REGION
    var leftFence: Boolean = false
    var rightFence: Boolean = false
    var upFence: Boolean = false
    var downFence: Boolean = false

    def fenceCnt: Int = Seq(leftFence, rightFence, upFence, downFence).count(_ == true)
  }

}
