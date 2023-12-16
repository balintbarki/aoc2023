package aoc.aoc2023.day16

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.Matrix

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

  override def calculatePart1(lines: Seq[String]): String = {
    val grid = Matrix.fromStrings(lines.toList)

    calculate(grid, 0, 0, Left).toString
  }

  override def calculatePart2(lines: Seq[String]): String = {
    val grid = Matrix.fromStrings(lines.toList)

    val startPositions =
      grid.rows.indices.map(rowIdx => (0, rowIdx, Left)) ++
        grid.rows.indices.map(rowIdx => (grid.columns.length - 1, rowIdx, Right)) ++
        grid.columns.indices.map(colIdx => (colIdx, 0, Up)) ++
        grid.columns.indices.map(colIdx => (colIdx, grid.rows.length - 1, Down))

    val values = startPositions.map {
      case (x, y, dir) => calculate(grid, x, y, dir)
    }

    values.max.toString
  }

  private def runLight(
    x: Int, y: Int, from: Direction, tiles: Matrix[Tile], height: Int, width: Int,
    jobQueue: mutable.Queue[() => Unit]): Unit = {

    val tile = tiles.get(x, y)

    def nextTileIsToUp(x: Int, y: Int): (Int, Int, Direction) = (x, y - 1, Down)

    def nextTileIsToDown(x: Int, y: Int): (Int, Int, Direction) = (x, y + 1, Up)

    def nextTileIsToLeft(x: Int, y: Int): (Int, Int, Direction) = (x - 1, y, Right)

    def nextTileIsToRight(x: Int, y: Int): (Int, Int, Direction) = (x + 1, y, Left)

    (from, tile) match {
      case (Left, t) if t.hasLeft   =>
      case (Right, t) if t.hasRight =>
      case (Up, t) if t.hasUp       =>
      case (Down, t) if t.hasDown   =>
      case _                        =>
        val nextDirections: Seq[(Int, Int, Direction)] = tile.processLight(from).map {
          case Up    => nextTileIsToUp(x, y)
          case Down  => nextTileIsToDown(x, y)
          case Left  => nextTileIsToLeft(x, y)
          case Right => nextTileIsToRight(x, y)
        }

        nextDirections.foreach { case (nextX, nextY, nextFrom) =>
          if ((0 <= nextX) && (nextX < width) && (0 <= nextY) && (nextY < height))
            jobQueue.enqueue(() => runLight(nextX, nextY, nextFrom, tiles, height: Int, width: Int, jobQueue))
        }
    }
  }

  private sealed trait Direction

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
      case Left =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Right)

      case Right =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Left)

      case Up =>
        this.hasUp = true
        this.hasDown = true
        Seq(Down)

      case Down =>
        this.hasUp = true
        this.hasDown = true
        Seq(Up)
    }
  }

  private case class LeftToUpMirror() extends Mirror {

    def symbol: Char = Tile.leftToUpSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Left =>
        this.hasLeft = true
        this.hasUp = true
        Seq(Up)

      case Right =>
        this.hasRight = true
        this.hasDown = true
        Seq(Down)

      case Up =>
        this.hasUp = true
        this.hasLeft = true
        Seq(Left)

      case Down =>
        this.hasRight = true
        this.hasDown = true
        Seq(Right)
    }
  }

  private case class LeftToDownMirror() extends Mirror {

    def symbol: Char = Tile.leftToDownSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Left =>
        this.hasLeft = true
        this.hasDown = true
        Seq(Down)

      case Right =>
        this.hasRight = true
        this.hasUp = true
        Seq(Up)

      case Up =>
        this.hasRight = true
        this.hasUp = true
        Seq(Right)

      case Down =>
        this.hasLeft = true
        this.hasDown = true
        Seq(Left)
    }
  }

  private case class HorizontalSplitter() extends Splitter {

    def symbol: Char = Tile.horizontalSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Left =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Right)

      case Right =>
        this.hasLeft = true
        this.hasRight = true
        Seq(Left)

      case Up =>
        this.hasUp = true
        this.hasLeft = true
        this.hasRight = true
        Seq(Left, Right)

      case Down =>
        this.hasDown = true
        this.hasLeft = true
        this.hasRight = true
        Seq(Left, Right)
    }
  }

  private case class VerticalSplitter() extends Splitter {

    def symbol: Char = Tile.verticalSymbol

    override def processLight(from: Direction): Seq[Direction] = from match {
      case Left =>
        this.hasLeft = true
        this.hasUp = true
        this.hasDown = true
        Seq(Up, Down)

      case Right =>
        this.hasRight = true
        this.hasUp = true
        this.hasDown = true
        Seq(Up, Down)

      case Up =>
        this.hasUp = true
        this.hasDown = true
        Seq(Down)

      case Down =>
        this.hasUp = true
        this.hasDown = true
        Seq(Up)
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

  private case object Up extends Direction

  private case object Down extends Direction

  private case object Left extends Direction

  private case object Right extends Direction


}
