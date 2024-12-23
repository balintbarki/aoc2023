package aoc.aoc2024.day16

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.{Direction, Matrix}

import scala.collection.mutable

case object Day16Puzzle extends DailyPuzzle2024(16, "Reindeer Maze") {

  val wallSymbol: Char = '#'
  val emptySymbol: Char = '.'
  val startSymbol: Char = 'S'
  val endSymbol: Char = 'E'

  override def calculatePart1(lines: Seq[String]): Long = {
    val maze = getInput(lines)

    val result = findCheapestPath(maze)

    result
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def findCheapestPath(maze: Matrix[MazeElement]): Long = {
    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue.empty
    val coordinateMap = maze.getCoordinateMap
    val ((startX, startY), _) = coordinateMap.find { case (_, element) => element == Start }
      .getOrElse(throw new IllegalArgumentException(s"Unexpected"))

    jobQueue.enqueue(() => findPathFrom(jobQueue, maze, startX, startY, Direction.Left))

    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }


    val end = maze.findOrThrow(_ == End)
    end.cheapestCostsFrom.values.min
  }

  private def findPathFrom(
    jobQueue: mutable.Queue[() => Unit], maze: Matrix[MazeElement], x: Int, y: Int, fromDir: Direction): Unit = {

    val currentCostFrom = maze.getOrThrow(x, y).getCheapestCostComingFrom(fromDir)
    val toDir = fromDir.opposite

    Seq(
      (toDir.rotateLeft, 1001),
      (toDir, 1),
      (toDir.rotateRight, 1001)).foreach { case (directionTo, stepCost) =>
      val comingFrom = directionTo.opposite
      val (neighborX, neighborY) = directionTo.adjustCoordinates(x, y)
      val neighborOpt = maze.get(neighborX, neighborY)
      neighborOpt match {
        case Some(neighbor: WalkableMazeElement) =>
          val newCost = if (Long.MaxValue - stepCost > currentCostFrom) currentCostFrom + stepCost else Long.MaxValue
          if (newCost < neighbor.getCheapestCostComingFrom(comingFrom)) {
            neighbor.cheapestCostsFrom.addOne(comingFrom, newCost)
            jobQueue.enqueue(() => findPathFrom(jobQueue, maze, neighborX, neighborY, directionTo.opposite))
          }

        case _ =>
      }
    }
  }

  private def getInput(lines: Seq[String]) = {
    Matrix(lines.map(line => line.map {
      case c if c == emptySymbol => new Empty
      case c if c == wallSymbol  => Wall
      case c if c == startSymbol => Start
      case c if c == endSymbol   => End
      case unexpected            => throw new IllegalArgumentException(s"Unexpected maze element: $unexpected")
    }))
  }


  private trait MazeElement {
    val cheapestCostsFrom: mutable.Map[Direction, Long] = {
      val map = mutable.Map.empty[Direction, Long]
      map.addOne(Direction.Up, Long.MaxValue)
      map.addOne(Direction.Down, Long.MaxValue)
      map.addOne(Direction.Left, Long.MaxValue)
      map.addOne(Direction.Right, Long.MaxValue)
      map
    }

    def getCheapestCostComingFrom(dir: Direction): Long = cheapestCostsFrom
      .getOrElse(dir, throw new IllegalArgumentException(s"Unexpected direction: $dir"))

    def symbol: Char
  }

  private trait WalkableMazeElement extends MazeElement


  private class Empty extends WalkableMazeElement {
    override def symbol: Char = emptySymbol
  }

  private object Wall extends MazeElement {
    override def symbol: Char = wallSymbol
  }

  private object Start extends WalkableMazeElement {
    cheapestCostsFrom.addOne(Direction.Up, 0)
    cheapestCostsFrom.addOne(Direction.Down, 0)
    cheapestCostsFrom.addOne(Direction.Left, 0)
    cheapestCostsFrom.addOne(Direction.Right, 0)

    override def symbol: Char = startSymbol
  }

  private object End extends WalkableMazeElement {
    override def symbol: Char = endSymbol
  }
}
