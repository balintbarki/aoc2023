package aoc.aoc2023.day10

import aoc.DailyPuzzle

case object Day10Puzzle extends DailyPuzzle(10, "unknown") {
  override def calculatePart1(
    lines: Seq[String]): String = {

    val allNodes = {
      for (y <- lines.indices; x <- lines.head.indices) yield (x, y) -> Node(lines(y)(x), x, y)
    }.toMap

    discoverNeighbors(allNodes, lines.head.length - 1, lines.length - 1)

    val start = allNodes.find { case (_, node) => node.isInstanceOf[S] }.getOrElse(???)
    var finished = false
    var newNeighbors = start._2.neighbors
    var nextDistance = 1
    while (!finished) {
      newNeighbors.foreach(newNeighbor => newNeighbor.distance = nextDistance)
      newNeighbors = newNeighbors.flatMap(_.neighbors).filter(_.distance < 0)
      finished = newNeighbors.forall(_.distance >= 0)
      nextDistance = nextDistance + 1
    }

    (nextDistance - 1).toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = ???

  def discoverNeighbors(nodeMap: Map[(Int, Int), Node], maxX: Int, maxY: Int): Unit = {

    def nodeAt(x: Int, y: Int): Node = nodeMap((x, y))

    nodeMap.foreach { case ((x, y), node) =>

      val isStartNode = node.isInstanceOf[S]

      if (0 < y) {
        val northNeighbor = nodeAt(x, y - 1)
        if (node.isInstanceOf[HasNorth] || (isStartNode && northNeighbor.isInstanceOf[HasSouth])) {
          node.addNeighbor(northNeighbor)
        }
      }

      if (y < maxY) {
        val southNeighbor = nodeAt(x, y + 1)
        if (node.isInstanceOf[HasSouth] || (isStartNode && southNeighbor.isInstanceOf[HasNorth])) {
          node.addNeighbor(southNeighbor)
        }
      }

      if (0 < x) {
        val westNeighbor = nodeAt(x - 1, y)
        if (node.isInstanceOf[HasWest] || (isStartNode && westNeighbor.isInstanceOf[HasEast])) {
          node.addNeighbor(westNeighbor)
        }
      }

      if (x < maxX) {
        val eastNeighbor = nodeAt(x + 1, y)
        if (node.isInstanceOf[HasEast] || (isStartNode && eastNeighbor.isInstanceOf[HasWest])) {
          node.addNeighbor(eastNeighbor)
        }
      }
    }
  }
}

class Node(val x: Int, val y: Int) {
  var distance: Int = -1
  var neighbors: Seq[Node] = Seq()

  def addNeighbor(node: Node): Unit = {
    this.neighbors = neighbors :+ node
  }
}

object Node {
  def apply(value: Char, x: Int, y: Int): Node = value match {
    case '|' => NorthSouth(x, y)
    case '-' => EastWest(x, y)
    case 'L' => NorthEast(x, y)
    case 'J' => NorthWest(x, y)
    case '7' => SouthWest(x, y)
    case 'F' => SouthEast(x, y)
    case '.' => Ground(x, y)
    case 'S' => S(x, y)
  }
}

trait HasNorth

trait HasSouth

trait HasEast

trait HasWest

case class NorthSouth(override val x: Int, override val y: Int) extends Node(x, y) with HasNorth with HasSouth

case class NorthEast(override val x: Int, override val y: Int) extends Node(x, y) with HasNorth with HasEast

case class NorthWest(override val x: Int, override val y: Int) extends Node(x, y) with HasNorth with HasWest

case class SouthEast(override val x: Int, override val y: Int) extends Node(x, y) with HasSouth with HasEast

case class SouthWest(override val x: Int, override val y: Int) extends Node(x, y) with HasSouth with HasWest

case class EastWest(override val x: Int, override val y: Int) extends Node(x, y) with HasEast with HasWest

case class Ground(override val x: Int, override val y: Int) extends Node(x, y = -1)

case class S(override val x: Int, override val y: Int) extends Node(x, y = 0)
