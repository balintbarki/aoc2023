package aoc.aoc2023.day25

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.graphs.{UndirectedGraph, UndirectedGraphNode}

import scala.collection.mutable

case object Day25Puzzle extends DailyPuzzle2023(25, "unknown") {
  override def calculatePart1(lines: Seq[String]): Long = {

    val graph = parseInput(lines)

    var (cutSize, firstGraph, secondGraph) = (10, UndirectedGraph(Set()), UndirectedGraph(Set()))

    do {
      graph.findMinimumCut() match {
        case (size, first, second) =>
          cutSize = size
          firstGraph = first
          secondGraph = second
          println(s"Cut size found: $cutSize")
      }
    } while (cutSize != 3)

    (firstGraph.nodes.size * secondGraph.nodes.size)
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def parseInput(lines: Seq[String]): UndirectedGraph = {
    val nodeMap: mutable.Map[String, UndirectedGraphNode] = mutable.Map()

    lines.foreach(line => line.split(": ").toSeq match {
      case Seq(source: String, targets: String) =>
        targets.split(" ").toSeq.foreach { target =>
          val sourceNode = nodeMap.getOrElseUpdate(source, UndirectedGraphNode(source))
          val targetNode = nodeMap.getOrElseUpdate(target, UndirectedGraphNode(target))
          sourceNode.connectTo(targetNode)
        }
    })

    UndirectedGraph(nodeMap.values.toSet)
  }

}
