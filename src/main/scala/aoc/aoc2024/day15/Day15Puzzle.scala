package aoc.aoc2024.day15

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.{Direction, Matrix, Vertical}

case object Day15Puzzle extends DailyPuzzle2024(15, "Warehouse Woes") {

  private val WallSymbol = '#'
  private val SingleBoxSymbol = 'O'
  private val BoxLeftHalfSymbol = '['
  private val BoxRightHalfSymbol = ']'
  private val EmptySymbol = '.'
  private val RobotSymbol = '@'

  override def calculatePart1(lines: Seq[String]): Long = {

    val (map, steps) = readInput(lines)
    performStepsAndGetCoordinatesSum(map, steps)
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val (originalMap, steps) = readInput(lines)
    val map = widenMapForPart2(originalMap)

    performStepsAndGetCoordinatesSum(map, steps)
  }

  private def performStepsAndGetCoordinatesSum(map: Matrix[MapElement], steps: Seq[Direction]): Long = {
    var robotPosition = findRobot(map)

    steps.foreach { step =>
      if (canPush(map, robotPosition, step)) {
        robotPosition = push(map, robotPosition, step)
      }
    }

    map.getCoordinateMap.filter { case ((_, _), element) => element.isInstanceOf[Box] }
      .flatMap { case ((x, y), element) =>
        val distances = element match {
          case SingleBox | BoxLeftHalf => Some((x, y))
          case _                       => None
        }
        distances.map { case (x, y) => x + y * 100 }
      }.sum
  }

  private def getNeighborCoordinates(position: (Int, Int), direction: Direction): (Int, Int) = {
    val (x, y) = position
    direction match {
      case Direction.Up    => (x, y - 1)
      case Direction.Down  => (x, y + 1)
      case Direction.Left  => (x - 1, y)
      case Direction.Right => (x + 1, y)
    }
  }

  private def canPush(map: Matrix[MapElement], robotPosition: (Int, Int), direction: Direction): Boolean = {
    val (neighborX, neighborY) = getNeighborCoordinates(robotPosition, direction)
    map.get(neighborX, neighborY) match {
      case Wall                                             => false
      case Empty                                            => true
      case BoxLeftHalf if direction.isInstanceOf[Vertical]  =>
        canPush(map, (neighborX, neighborY), direction) && canPush(map, (neighborX + 1, neighborY), direction)
      case BoxRightHalf if direction.isInstanceOf[Vertical] =>
        canPush(map, (neighborX, neighborY), direction) && canPush(map, (neighborX - 1, neighborY), direction)
      case _: Box                                           => canPush(map, (neighborX, neighborY),
        direction)
      case unexpected                                       => throw new IllegalArgumentException(
        s"Unexpected item in map: $unexpected")
    }
  }

  private def push(
    map: Matrix[MapElement], position: (Int, Int), direction: Direction): (Int, Int) = {
    val (x, y) = position
    val element = map.get(x, y)
    map.update(x, y, Empty)
    val (neighborX, neighborY) = getNeighborCoordinates(position, direction)
    map.get(neighborX, neighborY) match {
      case Empty                                            =>
      case BoxLeftHalf if direction.isInstanceOf[Vertical]  =>
        push(map, (neighborX, neighborY), direction)
        push(map, (neighborX + 1, neighborY), direction)
      case BoxRightHalf if direction.isInstanceOf[Vertical] =>
        push(map, (neighborX, neighborY), direction)
        push(map, (neighborX - 1, neighborY), direction)
      case _: Box                                           => push(map, (neighborX, neighborY),
        direction)
      case unexpected                                       => throw new IllegalArgumentException(
        s"Push attempted through unexpected item in map: $unexpected")
    }
    map.update(neighborX, neighborY, element)
    (neighborX, neighborY)
  }

  private def findRobot(map: Matrix[MapElement]): (Int, Int) = {
    map.getCoordinateMap.find { case ((_, _), element) => element == Robot }
      .map { case ((x, y), _) => (x, y) }.getOrElse(throw new IllegalArgumentException(s"Robot not found"))
  }

  private def readInput(lines: Seq[String]): (Matrix[MapElement], Seq[Direction]) = {
    lines.span(_.nonEmpty) match {
      case (mapLines, stepLines) =>
        val map = mapLines.map(line => line.map {
          case WallSymbol      => Wall
          case SingleBoxSymbol => SingleBox
          case EmptySymbol     => Empty
          case RobotSymbol     => Robot
          case unexpected      => throw new IllegalArgumentException(s"Unexpected char in input: $unexpected")
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

  private def widenMapForPart2(map: Matrix[MapElement]): Matrix[MapElement] = {
    Matrix(map.rows.map { row =>
      row.flatMap {
        case Wall      => Seq.fill(2)(Wall)
        case Empty     => Seq.fill(2)(Empty)
        case SingleBox => Seq(BoxLeftHalf, BoxRightHalf)
        case Robot     => Seq(Robot, Empty)
      }
    })
  }

  private trait MapElement {
    def symbol: Char

    override def toString: String = symbol.toString
  }

  private trait Box extends MapElement

  private object Wall extends MapElement {
    override def symbol: Char = WallSymbol
  }

  private object Empty extends MapElement {
    override def symbol: Char = EmptySymbol
  }

  private object SingleBox extends Box {
    override def symbol: Char = SingleBoxSymbol
  }

  private object BoxLeftHalf extends Box {
    override def symbol: Char = BoxLeftHalfSymbol
  }

  private object BoxRightHalf extends Box {
    override def symbol: Char = BoxRightHalfSymbol
  }

  private object Robot extends MapElement {
    override def symbol: Char = RobotSymbol
  }

}
