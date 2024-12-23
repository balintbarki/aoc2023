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
    val result = findCheapestPaths(maze)

    result
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val maze = getInput(lines)
    val cheapestPathPrice = findCheapestPaths(maze)

    val end = maze.findOrThrow(_ == End)


    val elementsInCheapestPaths = end.cheapestCostsFrom.filter { case (_, cost) => cost == cheapestPathPrice }.keys
      .flatMap { key => end.cheapestPathsFrom(key) }.flatten.toSeq.distinct

    // Account for the End item as well
    elementsInCheapestPaths.size + 1
  }

  private def findCheapestPaths(maze: Matrix[MazeElement]): Long = {
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

    val element = maze.getOrThrow(x, y)
    val currentCostFrom = maze.getOrThrow(x, y).getCheapestCostComingFrom(fromDir)
    val toDir = fromDir.opposite

    Seq(
      (toDir.rotateLeft, 1001),
      (toDir, 1),
      (toDir.rotateRight, 1001)).foreach { case (directionTo, stepCost) =>

      val (neighborX, neighborY) = directionTo.adjustCoordinates(x, y)
      val neighborOpt = maze.get(neighborX, neighborY)
      neighborOpt match {
        case Some(neighbor: WalkableMazeElement) =>
          val newCost = if (Long.MaxValue - stepCost > currentCostFrom) currentCostFrom + stepCost else Long.MaxValue
          val comingFrom = directionTo.opposite
          val neighborCheapestCostFrom = neighbor.getCheapestCostComingFrom(comingFrom)
          val cheapestPathsToAdd = element.cheapestPathsFrom(fromDir).map(_ ++ mutable.Seq(element))
          if (newCost < neighborCheapestCostFrom) {
            neighbor.cheapestCostsFrom.addOne(comingFrom, newCost)

            // Update the cheapest path with the new one
            neighbor.cheapestPathsFrom.addOne(comingFrom, cheapestPathsToAdd)

            jobQueue.enqueue(() => findPathFrom(jobQueue, maze, neighborX, neighborY, comingFrom))
          }
          else if (newCost == neighborCheapestCostFrom) {
            val neighborCheapestPathFrom = neighbor.cheapestPathsFrom(comingFrom)
            // Add the cheapest path to the existing ones
            neighbor.cheapestPathsFrom.update(comingFrom, neighborCheapestPathFrom ++ cheapestPathsToAdd)
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

    val cheapestPathsFrom: mutable.Map[Direction, mutable.Seq[mutable.Seq[MazeElement]]] = {
      val map = mutable.Map.empty[Direction, mutable.Seq[mutable.Seq[MazeElement]]]
      map.addOne(Direction.Up, mutable.Seq.empty)
      map.addOne(Direction.Down, mutable.Seq.empty)
      map.addOne(Direction.Left, mutable.Seq.empty)
      map.addOne(Direction.Right, mutable.Seq.empty)
      map
    }

    var partOfCheapestPath: Boolean = false

    def getCheapestCostComingFrom(dir: Direction): Long = cheapestCostsFrom
      .getOrElse(dir, throw new IllegalArgumentException(s"Unexpected direction: $dir"))

    def symbol: Char

    override def toString: String = if (partOfCheapestPath) "O" else symbol.toString
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

    cheapestPathsFrom.addOne(Direction.Up, mutable.Seq(mutable.Seq.empty))
    cheapestPathsFrom.addOne(Direction.Down, mutable.Seq(mutable.Seq.empty))
    cheapestPathsFrom.addOne(Direction.Left, mutable.Seq(mutable.Seq.empty))
    cheapestPathsFrom.addOne(Direction.Right, mutable.Seq(mutable.Seq.empty))

    partOfCheapestPath = true

    override def symbol: Char = startSymbol
  }

  private object End extends WalkableMazeElement {
    partOfCheapestPath = true

    override def symbol: Char = endSymbol
  }
}
