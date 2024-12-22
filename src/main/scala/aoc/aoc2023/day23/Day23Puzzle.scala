package aoc.aoc2023.day23

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.graphs.{DirectedGraph, DirectedGraphNode}
import aoc.utils.{Direction, Matrix}

import scala.collection.mutable

case object Day23Puzzle extends DailyPuzzle2023(23, "A Long Walk") {
  override def calculatePart1(lines: Seq[String]): Long = {
    calculate(lines, isDirected = true)
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    calculate(lines, isDirected = false)
  }

  private def calculate(lines: Seq[String], isDirected: Boolean): Long = {
    val entry: DirectedGraphNode = DirectedGraphNode("Entry")
    val exit: DirectedGraphNode = DirectedGraphNode("Exit")

    val matrix = Matrix(lines.map(line => line.map(c => Tile(c))))
    val tileMap = matrix.getCoordinateMap

    val startCoordinates = (lines.head.indexOf("."), 0)
    val endCoordinates = (lines.last.indexOf("."), lines.length - 1)

    val nodeMap: mutable.Map[(Int, Int), DirectedGraphNode] = mutable.Map(
      startCoordinates -> entry,
      endCoordinates -> exit
    )

    discoverNodes(startCoordinates._1, startCoordinates._2, Direction.Down, entry, tileMap, nodeMap, startCoordinates,
      endCoordinates,
      isDirected)

    val graph = new DirectedGraph(nodeMap.values.toList)
    graph.getLongestPath(entry, exit)
  }

  private def discoverNodes(
    x: Int, y: Int, dir: Direction, fromNode: DirectedGraphNode, tileMap: Map[(Int, Int), Tile],
    nodeMap: mutable.Map[(Int, Int), DirectedGraphNode],
    startCoordinates: (Int, Int), endCoordinates: (Int, Int), isDirected: Boolean): Unit = {

    var nodeFound = false
    var weight = 1
    var lastDir = dir

    var (newX, newY) = dir.adjustCoordinates(x, y)

    while (!nodeFound) {
      //println(s" at $newX,$newY, weight $weight")

      if ((newX, newY) == startCoordinates) {
        nodeFound = true
      }
      else if ((newX, newY) == endCoordinates) {
        val exitNode = nodeMap(endCoordinates)
        if (!fromNode.isConnectedTo(exitNode))
          fromNode.connectTo(exitNode, weight)
        nodeFound = true
      }
      else {
        // Check all directions at the new location (including incoming slopes) to decide if this is a node
        val allDirections = lastDir.opposite.restOfDirs.filter { newDir =>
          tileMap(newDir.adjustCoordinates(newX, newY)) match {
            case Forest => false
            case _      => true
          }
        }

        // Check the possible directions from the new location
        val possibleDirections = lastDir.opposite.restOfDirs.filter { newDir =>
          val tile = tileMap(newDir.adjustCoordinates(newX, newY))
          tile match {
            case Path            => true
            case Slope(slopeDir) => (slopeDir == newDir) || !isDirected
            case _               => false
          }
        }

        if (allDirections.length > 1) {
          // A node is found
          //println(s"Node found at $newX,$newY")
          nodeFound = true
          val newNode = nodeMap.getOrElse((newX, newY), DirectedGraphNode(s"N${newX}_$newY"))
          nodeMap.update((newX, newY), newNode)

          if (!fromNode.isConnectedTo(newNode)) {
            fromNode.connectTo(newNode, weight)
            possibleDirections.foreach { newDir =>
              discoverNodes(newX, newY, newDir, newNode, tileMap, nodeMap, startCoordinates, endCoordinates, isDirected)
            }
          }
        }
        else if (possibleDirections.length == 1) {
          lastDir = possibleDirections.head
          lastDir.adjustCoordinates(newX, newY) match {
            case (x, y) => newX = x; newY = y
          }
          weight = weight + 1
        }
        else {
          // No possible directions, not sure if this is supposed to happen
          throw new IllegalArgumentException(s"Unexpected end of path at $newX, $newY")
        }
      }
    }
  }

  private abstract class Tile

  private abstract class AbstractSlope extends Tile

  private case class Slope(dir: Direction) extends AbstractSlope

  private case object Path extends Tile

  private object Tile {
    def apply(c: Char): Tile = c match {
      case '.' => Path
      case '#' => Forest
      case '<' => Slope(Direction.Left)
      case '>' => Slope(Direction.Right)
      case 'v' => Slope(Direction.Down)
      case '^' => Slope(Direction.Up)
    }
  }

  private case object Forest extends Tile

}
