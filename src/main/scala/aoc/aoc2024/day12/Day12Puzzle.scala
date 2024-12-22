package aoc.aoc2024.day12

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.Matrix

case object Day12Puzzle extends DailyPuzzle2024(12, "Garden Groups") {

  val UNKNOWN_REGION = 0

  override def calculatePart1(lines: Seq[String]): Long = {

    val garden = createGarden(lines)

    val allPlots = garden.flattenedElements
    allPlots.map(_.regionId).distinct.map { regionId =>
      val plotsInRegion = allPlots.filter(_.regionId == regionId)
      plotsInRegion.map(_.fenceCnt).sum * plotsInRegion.size
    }.sum
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val garden = createGarden(lines)

    val allPlots = garden.flattenedElements
    allPlots.map(_.regionId).distinct.map { regionId =>
      val plotsInRegion = allPlots.filter(_.regionId == regionId)
      val cornerCnt = plotsInRegion.map(plot => plot.cornerCnt).sum
      cornerCnt * plotsInRegion.size
    }.sum
  }

  private def createGarden(lines: Seq[String]): Matrix[Plot] = {
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

    garden
  }

  private def setUpFences(plotCoordinateMap: Map[(Int, Int), Plot], maxX: Int, maxY: Int): Unit = {
    plotCoordinateMap.foreach { case ((x, y), plot) =>

      if (0 < x)
        plot.leftNeighbor = plotCoordinateMap((x - 1, y))

      if (x < maxX)
        plot.rightNeighbor = plotCoordinateMap((x + 1, y))

      if (0 < y)
        plot.upNeighbor = plotCoordinateMap((x, y - 1))

      if (y < maxY)
        plot.downNeighbor = plotCoordinateMap((x, y + 1))
    }
  }

  private def readGarden(lines: Seq[String]) =
    Matrix(lines.map(line => line.map { c => new Plot(c) }))


  private class Plot(val plantId: Char) {
    var regionId: Int = UNKNOWN_REGION
    var leftNeighbor: Plot = OutOfGarden
    var rightNeighbor: Plot = OutOfGarden
    var upNeighbor: Plot = OutOfGarden
    var downNeighbor: Plot = OutOfGarden

    def leftFence: Boolean = neighborInDifferentRegion(leftNeighbor)

    def rightFence: Boolean = neighborInDifferentRegion(rightNeighbor)

    def upFence: Boolean = neighborInDifferentRegion(upNeighbor)

    def downFence: Boolean = neighborInDifferentRegion(downNeighbor)

    def fenceCnt: Int = Seq(
      leftFence,
      rightFence,
      upFence,
      downFence).count(_ == true)

    def cornerCnt: Int = Seq(
      leftFence && upFence,
      leftFence && downFence,
      rightFence && upFence,
      rightFence && downFence,
      neighborInSameRegion(leftNeighbor) && neighborInSameRegion(upNeighbor) && leftNeighbor.upFence && upNeighbor
        .leftFence,
      neighborInSameRegion(leftNeighbor) && neighborInSameRegion(downNeighbor) && leftNeighbor.downFence && downNeighbor
        .leftFence,
      neighborInSameRegion(rightNeighbor) && neighborInSameRegion(upNeighbor) && rightNeighbor.upFence && upNeighbor
        .rightFence,
      neighborInSameRegion(rightNeighbor) && neighborInSameRegion(downNeighbor) && rightNeighbor
        .downFence && downNeighbor.rightFence,

    ).count(_ == true)

    private def neighborInSameRegion(neighbor: Plot) = neighbor.regionId == regionId

    private def neighborInDifferentRegion(neighbor: Plot) = !neighborInSameRegion(neighbor)
  }

  private object OutOfGarden extends Plot('.')

}
