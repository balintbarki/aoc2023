package aoc.aoc2023.day16

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.{Direction, Matrix}

import scala.collection.mutable

case object Day16Puzzle extends DailyPuzzle2023(16, "The Floor Will Be Lava") {

  def calculate(grid: Matrix[Char], x: Int, y: Int, from: Direction): Int = {
    val tiles: Matrix[Tile] = grid.map(c => Tile(c))

    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

    jobQueue.enqueue(() => runLight(x, y, from, tiles, grid.rows.length, grid.columns.length, jobQueue))
    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }

    tiles.count(_.hasLight)
  }

  override def calculatePart1(lines: Seq[String]): Long = {
    val grid = Matrix.fromStrings(lines)

    calculate(grid, 0, 0, Direction.Left)
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val grid = Matrix.fromStrings(lines)

    val startPositions =
      grid.rows.indices.map(rowIdx => (0, rowIdx, Direction.Left)) ++
        grid.rows.indices.map(rowIdx => (grid.columns.length - 1, rowIdx, Direction.Right)) ++
        grid.columns.indices.map(colIdx => (colIdx, 0, Direction.Up)) ++
        grid.columns.indices.map(colIdx => (colIdx, grid.rows.length - 1, Direction.Down))

    val values = startPositions.map {
      case (x, y, dir) => calculate(grid, x, y, dir)
    }

    values.max
  }

  private def runLight(
    x: Int, y: Int, from: Direction, tiles: Matrix[Tile], height: Int, width: Int,
    jobQueue: mutable.Queue[() => Unit]): Unit = {

    val tile = tiles.get(x, y)

    def nextTileIsToUp(x: Int, y: Int): (Int, Int, Direction) = (x, y - 1, Direction.Down)

    def nextTileIsToDown(x: Int, y: Int): (Int, Int, Direction) = (x, y + 1, Direction.Up)

    def nextTileIsToLeft(x: Int, y: Int): (Int, Int, Direction) = (x - 1, y, Direction.Right)

    def nextTileIsToRight(x: Int, y: Int): (Int, Int, Direction) = (x + 1, y, Direction.Left)

    (from, tile) match {
      case (Direction.Left, t) if t.hasLeft   =>
      case (Direction.Right, t) if t.hasRight =>
      case (Direction.Up, t) if t.hasUp       =>
      case (Direction.Down, t) if t.hasDown   =>
      case _                                  =>
        val nextDirections: Seq[(Int, Int, Direction)] = tile.processLight(from).map {
          case Direction.Up    => nextTileIsToUp(x, y)
          case Direction.Down  => nextTileIsToDown(x, y)
          case Direction.Left  => nextTileIsToLeft(x, y)
          case Direction.Right => nextTileIsToRight(x, y)
        }

        nextDirections.foreach { case (nextX, nextY, nextFrom) =>
          if ((0 <= nextX) && (nextX < width) && (0 <= nextY) && (nextY < height))
            jobQueue.enqueue(() => runLight(nextX, nextY, nextFrom, tiles, height: Int, width: Int, jobQueue))
        }
    }
  }

  private abstract class Tile {
    var hasLeft: Boolean = false
    var hasRight: Boolean = false
    var hasUp: Boolean = false
    var hasDown: Boolean = false

    def symbol: Char

    def hasLight: Boolean = hasLeft || hasRight || hasUp || hasDown

    def processLight(from: Direction): Seq[Direction]
  }

  private abstract class Mirror extends Tile

  private abstract class Splitter extends Tile

  private case class Empty() extends Tile {

    def symbol: Char = Tile.emptySymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Direction.Left =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Right)

      case Direction.Right =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Left)

      case Direction.Up =>
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Down)

      case Direction.Down =>
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Up)
    }
  }

  private case class LeftToUpMirror() extends Mirror {

    def symbol: Char = Tile.leftToUpSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Direction.Left =>
        this.hasLeft = true
        this.hasUp = true
        Seq(Direction.Up)

      case Direction.Right =>
        this.hasRight = true
        this.hasDown = true
        Seq(Direction.Down)

      case Direction.Up =>
        this.hasUp = true
        this.hasLeft = true
        Seq(Direction.Left)

      case Direction.Down =>
        this.hasRight = true
        this.hasDown = true
        Seq(Direction.Right)
    }
  }

  private case class LeftToDownMirror() extends Mirror {

    def symbol: Char = Tile.leftToDownSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Direction.Left =>
        this.hasLeft = true
        this.hasDown = true
        Seq(Direction.Down)

      case Direction.Right =>
        this.hasRight = true
        this.hasUp = true
        Seq(Direction.Up)

      case Direction.Up =>
        this.hasRight = true
        this.hasUp = true
        Seq(Direction.Right)

      case Direction.Down =>
        this.hasLeft = true
        this.hasDown = true
        Seq(Direction.Left)
    }
  }

  private case class HorizontalSplitter() extends Splitter {

    def symbol: Char = Tile.horizontalSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Direction.Left =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Right)

      case Direction.Right =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Left)

      case Direction.Up =>
        this.hasUp = true
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Left, Direction.Right)

      case Direction.Down =>
        this.hasDown = true
        this.hasLeft = true
        this.hasRight = true
        Seq(Direction.Left, Direction.Right)
    }
  }

  private case class VerticalSplitter() extends Splitter {

    def symbol: Char = Tile.verticalSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Direction.Left =>
        this.hasLeft = true
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Up, Direction.Down)

      case Direction.Right =>
        this.hasRight = true
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Up, Direction.Down)

      case Direction.Up =>
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Down)

      case Direction.Down =>
        this.hasUp = true
        this.hasDown = true
        Seq(Direction.Up)
    }
  }

  private object Tile {

    val emptySymbol = '.'
    val leftToUpSymbol = '/'
    val leftToDownSymbol = '\\'
    val horizontalSymbol = '-'
    val verticalSymbol = '|'

    def apply(c: Char): Tile = c match {
      case '.'  => Empty()
      case '/'  => LeftToUpMirror()
      case '\\' => LeftToDownMirror()
      case '-'  => HorizontalSplitter()
      case '|'  => VerticalSplitter()
      case _    => ???
    }
  }
}
