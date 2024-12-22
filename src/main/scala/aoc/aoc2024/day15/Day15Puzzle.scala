package aoc.aoc2024.day15

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.{Direction, Matrix}

case object Day15Puzzle extends DailyPuzzle2024(15, "Warehouse Woes") {

  private val WallSymbol = '#'
  private val BoxSymbol = 'O'
  private val EmptySymbol = '.'
  private val RobotSymbol = '@'

  override def calculatePart1(lines: Seq[String]): Long = {
    var (map, steps) = readInput(lines)

    var robotPosition = findRobot(map)

    val startDirection = Direction.Right

    var currentDirection: Direction = startDirection

    steps.foreach { step =>
      val (newMap, newRobotPosition, newDirection) = performStep(map, robotPosition, currentDirection, step)
      map = newMap
      robotPosition = newRobotPosition
      currentDirection = newDirection
    }

    val reorientedMap = mapTransformBetweenSteps(map, currentDirection, startDirection)

    reorientedMap.getCoordinateMap.filter { case ((_, _), element) => element == Box }
      .map { case ((x, y), _) => x + 100 * y }.sum
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def stepToRight(map: Matrix[MapElement], robotPosition: (Int, Int)): (Matrix[MapElement], (Int, Int)) = {
    val (robotX, robotY) = robotPosition
    val row = map.elements(robotY)
    // We can assume that there will always be a wall
    val firstWallRightToRobot = row.indexOf(Wall, robotX)
    val firstEmptyRightToRobot = row.indexOf(Empty, robotX)
    val newRobotX = if (0 <= firstEmptyRightToRobot) {
      if (firstEmptyRightToRobot < firstWallRightToRobot) {
        val itemsToShift = row.slice(robotX, firstEmptyRightToRobot)
        row.update(robotX, Empty)
        itemsToShift.indices.foreach { i => row.update(robotX + 1 + i, itemsToShift(i)) }
        robotX + 1
      }
      else {
        robotX
      }
    } else {
      robotX
    }

    (map, (newRobotX, robotY))
  }

  private def findRobot(map: Matrix[MapElement]): (Int, Int) = {
    map.getCoordinateMap.find { case ((_, _), element) => element == Robot }
      .map { case ((x, y), _) => (x, y) }.getOrElse(throw new IllegalArgumentException(s"Robot not found"))
  }

  private def performStep(
    map: Matrix[MapElement], robotPosition: (Int, Int), currentDirection: Direction,
    step: Direction): (Matrix[MapElement], (Int, Int), Direction) = {

    val transformedMap = mapTransformBetweenSteps(map, currentDirection, step)
    val transformedRobotPosition = robotTransformBetweenSteps(robotPosition, map.xSize - 1, map.ySize - 1,
      currentDirection, step)
    val (mapAfterStep, robotAfterStep) = stepToRight(transformedMap, transformedRobotPosition)
    (mapAfterStep, robotAfterStep, step)
  }

  private def robotTransformBetweenSteps(
    robotPosition: (Int, Int), xMax: Int, yMax: Int, prev: Direction, next: Direction): (Int, Int) = {
    (prev, next) match {
      case (p, n) if p == n             => robotPosition // Nothing to do
      case (p, n) if n == p.opposite    => rotateLeftRobotPosition(rotateLeftRobotPosition(robotPosition, xMax), xMax)
      case (p, n) if p.rotateLeft == n  => rotateRightRobotPosition(robotPosition, yMax)
      case (p, n) if p.rotateRight == n => rotateLeftRobotPosition(robotPosition, xMax)
      case unexpected                   => throw new IllegalArgumentException(
        s"Unexpected directions: $unexpected")
    }
  }

  private def rotateLeftRobotPosition(position: (Int, Int), xMax: Int) = (position._2, xMax - position._1)

  private def rotateRightRobotPosition(position: (Int, Int), yMax: Int) = (yMax - position._2, position._1)

  private def mapTransformBetweenSteps(
    map: Matrix[MapElement], prev: Direction, next: Direction): Matrix[MapElement] = {
    (prev, next) match {
      case (p, n) if p == n             => map // Nothing to do
      case (p, n) if (n == p.opposite)  => map.rotateRight.rotateRight
      case (p, n) if p.rotateLeft == n  => map.rotateRight
      case (p, n) if p.rotateRight == n => map.rotateLeft
      case unexpected                   => throw new IllegalArgumentException(
        s"Unexpected directions: $unexpected")
    }
  }

  private def readInput(lines: Seq[String]): (Matrix[MapElement], Seq[Direction]) = {
    lines.span(_.nonEmpty) match {
      case (mapLines, stepLines) =>
        val map = mapLines.map(line => line.map {
          case WallSymbol  => Wall
          case BoxSymbol   => Box
          case EmptySymbol => Empty
          case RobotSymbol => Robot
          case unexpected  => throw new IllegalArgumentException(s"Unexpected char in input: $unexpected")
        })

        val steps = stepLines.flatMap(line => line.flatMap {
          case '<' => Some(Direction.Left)
          case '>' => Some(Direction.Right)
          case '^' => Some(Direction.Up)
          case 'v' => Some(Direction.Down)
          case _   => None
        })

        (Matrix(map), steps)
    }
  }

  private trait MapElement {
    def symbol: Char

    override def toString = symbol.toString
  }

  private object Wall extends MapElement {
    def symbol: Char = WallSymbol
  }

  private object Empty extends MapElement {
    def symbol: Char = EmptySymbol
  }

  private object Box extends MapElement {
    def symbol: Char = BoxSymbol
  }

  private object Robot extends MapElement {
    def symbol: Char = RobotSymbol
  }

}
