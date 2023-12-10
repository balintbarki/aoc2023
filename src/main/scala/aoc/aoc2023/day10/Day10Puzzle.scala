package aoc.aoc2023.day10

import aoc.DailyPuzzle

case object Day10Puzzle extends DailyPuzzle(10, "Pipe Maze") {
  override def calculatePart1(
    lines: Seq[String]): String = {

    val allNodes = createNodeMap(lines)

    discoverNeighbors(allNodes)
    walkPipeAndGetFarthestDistance(allNodes).toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = {

    val nodeMap = createNodeMap(lines)

    discoverNeighbors(nodeMap)
    walkPipeAndGetFarthestDistance(nodeMap)
    val stretchedNodeMap = strechNodeMap(nodeMap)

    // Find a node at the edges that is not part of the loop and in not a stretched node, and starting from that,
    // walk through all the neighbors that are not part of the loop and not stretched, and mark them as external
    // until there are no more nodes
    // Then count the total number of internal nodes which are not part of the loop and are not stretched
    val maxX = maxXOfNodeMap(stretchedNodeMap)
    val maxY = maxYOfNodeMap(stretchedNodeMap)
    val edgeNodes = stretchedNodeMap
      .filter { case ((x, y), node) => ((x == 0) || (y == 0) || (x == maxX) || (y == maxY)) && !node.isPartOfLoop
      }

    var externalNodes = edgeNodes.map { case (_, node) => node }.toSeq
    var stepCnt = 0

    while (externalNodes.nonEmpty && (stepCnt < stretchedNodeMap.size)) {

      stepCnt = stepCnt + 1

      externalNodes.foreach(node => {
        node.isWalked = true
        node.symbol = 'O'
      })

      val notProcessedNeighbors = externalNodes.flatMap { node => {
        getAllNeighbors(node, stretchedNodeMap, maxX, maxY).filter(node => !node.isPartOfLoop && !node.isWalked)
      }
      }

      externalNodes = notProcessedNeighbors
    }

    stretchedNodeMap.foreach { case (_, node) => if (!node.isWalked && !node.isPartOfLoop) node.symbol = 'I' }

    nodeMap.count { case (_, node) => !node.isWalked && !node.isStretched && !node.isPartOfLoop }.toString
  }

  def printNodeMap(nodeMap: Map[(Int, Int), Node]): Unit = {

    val maxX = maxXOfNodeMap(nodeMap)
    val maxY = maxYOfNodeMap(nodeMap)

    val nodes: Seq[Seq[Node]] = (0 to maxY).map { y =>
      (0 to maxX).map { x => nodeMap((x, y)) }
    }

    nodes.foreach(line => {
      line.foreach(node => print(node.symbol))
      println()
    })
  }

  private def discoverNeighbors(nodeMap: Map[(Int, Int), Node]): Unit = {

    def nodeAt(x: Int, y: Int): Node = nodeMap((x, y))

    val maxX = maxXOfNodeMap(nodeMap)
    val maxY = maxYOfNodeMap(nodeMap)

    nodeMap.foreach { case ((x, y), node) =>

      val isStartNode = node.isInstanceOf[S]

      if (0 < y) {
        val northNeighbor = nodeAt(x, y - 1)
        if (node.hasNorth || (isStartNode && northNeighbor.hasSouth)) {
          node.addConnection(northNeighbor)
          node.hasNorth = true
        }
      }

      if (y < maxY) {
        val southNeighbor = nodeAt(x, y + 1)
        if (node.hasSouth || (isStartNode && southNeighbor.hasNorth)) {
          node.addConnection(southNeighbor)
          node.hasSouth = true
        }
      }

      if (0 < x) {
        val westNeighbor = nodeAt(x - 1, y)
        if (node.hasWest || (isStartNode && westNeighbor.hasEast)) {
          node.addConnection(westNeighbor)
          node.hasWest = true
        }
      }

      if (x < maxX) {
        val eastNeighbor = nodeAt(x + 1, y)
        if (node.hasEast || (isStartNode && eastNeighbor.hasWest)) {
          node.addConnection(eastNeighbor)
          node.hasEast = true
        }
      }
    }
  }

  private def strechNodeMap(nodeMap: Map[(Int, Int), Node]): Map[(Int, Int), Node] = {
    val maxX = maxXOfNodeMap(nodeMap)
    val maxY = maxYOfNodeMap(nodeMap)

    val horizonalStretchResult = (0 to maxY).map { y =>
      val nodesInLine = (0 to maxX).map { x => nodeMap((x, y)) }

      nodesInLine.last.x = nodesInLine.last.x * 2
      // Insert the appropriate node
      nodesInLine.sliding(2).foldLeft(Seq[Node]())({ case (resultSeq, Seq(western, eastern)) =>
        western.x = western.x * 2
        val insertedX = western.x + 1
        val insertedNode = if (western.hasEast && eastern.hasWest) {
          val newNode = new EastWest(insertedX, y)
          newNode.distance = western.distance
          newNode
        }
        else
          new Ground(insertedX, y)
        insertedNode.isStretched = true
        resultSeq ++ Seq(western, insertedNode)
      }) :+ nodesInLine.last
    }

    horizonalStretchResult.last.foreach(node => node.y = node.y * 2)

    val verticalStrechResult = horizonalStretchResult.sliding(2)
      .foldLeft(Seq[Seq[Node]]()) { case (resultSeq, Seq(northLine, southLine)) =>
        val insertedLine = northLine.zip(southLine).map { case (northern, southern) =>
          northern.y = northern.y * 2
          val insertedY = northern.y + 1
          val insertedNode = if (northern.hasSouth && southern.hasNorth) {
            val newNode = new NorthSouth(northern.x, insertedY)
            newNode.distance = northern.distance
            newNode
          }
          else
            new Ground(northern.x, insertedY)
          insertedNode.isStretched = true
          insertedNode
        }

        resultSeq ++ Seq(northLine, insertedLine)
      } :+ horizonalStretchResult.last

    verticalStrechResult.flatMap { line => {
      line.map { node => (node.x, node.y) -> node }
    }
    }.toMap
  }

  private def getAllNeighbors(node: Node, nodeMap: Map[(Int, Int), Node], maxX: Int, maxY: Int): Seq[Node] = {

    val northern = if (0 < node.y)
      Some(nodeMap(node.x, node.y - 1))
    else
      None

    val southern = if (node.y < maxY)
      Some(nodeMap(node.x, node.y + 1))
    else
      None

    val eastern = if (node.x < maxX)
      Some(nodeMap(node.x + 1, node.y))
    else
      None

    val western = if (0 < node.x)
      Some(nodeMap(node.x - 1, node.y))
    else
      None

    Seq(northern, southern, eastern, western).flatten
  }

  private def maxXOfNodeMap(nodeMap: Map[(Int, Int), Node]): Int = nodeMap.map { case ((x, _), _) => x }.max

  private def maxYOfNodeMap(nodeMap: Map[(Int, Int), Node]): Int = nodeMap.map { case ((_, y), _) => y }.max

  private def createNodeMap(lines: Seq[String]): Map[(Int, Int), Node] = {
    {
      for (y <- lines.indices; x <- lines.head.indices) yield (x, y) -> Node(lines(y)(x), x, y)
    }.toMap
  }

  private def walkPipeAndGetFarthestDistance(nodeMap: Map[(Int, Int), Node]): Int = {
    val start = nodeMap.find { case (_, node) => node.isInstanceOf[S] }.getOrElse(???)
    var finished = false
    var newNeighbors = start._2.connectedNodes
    var nextDistance = 1
    while (!finished) {
      newNeighbors.foreach(_.distance = nextDistance)
      newNeighbors = newNeighbors.flatMap(_.connectedNodes).filter(_.distance < 0)
      finished = newNeighbors.forall(_.distance >= 0)
      nextDistance = nextDistance + 1
    }

    nextDistance - 1
  }
}

abstract class Node(var x: Int, var y: Int) {
  // If negative, this also represents that a tile is not in the loop
  var distance: Int = -1
  var connectedNodes: Seq[Node] = Seq()
  var hasNorth: Boolean = false
  var hasSouth: Boolean = false
  var hasWest: Boolean = false
  var hasEast: Boolean = false
  var isStretched: Boolean = false
  var isWalked: Boolean = false
  var symbol: Char = '?'

  def isPartOfLoop: Boolean = 0 <= distance

  def addConnection(node: Node): Unit = {
    this.connectedNodes = connectedNodes :+ node
  }
}

object Node {
  def apply(value: Char, x: Int, y: Int): Node = value match {
    case '|' => new NorthSouth(x, y)
    case '-' => new EastWest(x, y)
    case 'L' => new NorthEast(x, y)
    case 'J' => new NorthWest(x, y)
    case '7' => new SouthWest(x, y)
    case 'F' => new SouthEast(x, y)
    case '.' => new Ground(x, y)
    case 'S' => new S(x, y)
  }
}

class NorthSouth(x: Int, y: Int) extends Node(x, y) {
  this.symbol = '|'
  this.hasNorth = true
  this.hasSouth = true
}

class NorthEast(x: Int, y: Int) extends Node(x, y) {
  this.symbol = 'L'
  this.hasNorth = true
  this.hasEast = true
}

class NorthWest(x: Int, y: Int) extends Node(x, y) {
  this.symbol = 'J'
  this.hasNorth = true
  this.hasWest = true
}

class SouthEast(x: Int, y: Int) extends Node(x, y) {
  this.symbol = 'F'
  this.hasSouth = true
  this.hasEast = true
}

class SouthWest(x: Int, y: Int) extends Node(x, y) {
  this.symbol = '7'
  this.hasSouth = true
  this.hasWest = true
}

class EastWest(x: Int, y: Int) extends Node(x, y) {
  this.symbol = '-'
  this.hasEast = true
  this.hasWest = true
}

class Ground(x: Int, y: Int) extends Node(x, y) {
  this.symbol = '.'
}

class S(x: Int, y: Int) extends Node(x, y) {
  this.symbol = 'S'
  this.distance = 0
}
