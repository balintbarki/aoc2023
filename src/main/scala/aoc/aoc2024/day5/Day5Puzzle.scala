package aoc.aoc2024.day5

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.graphs.{DirectedGraph, DirectedGraphNode}

import scala.collection.mutable

case object Day5Puzzle extends DailyPuzzle2024(5, "Print Queue") {

  override def calculatePart1(lines: Seq[String]): String = {
    getMiddleElementSumForOrderedPageLists(lines, true).toString
  }

  override def calculatePart2(lines: Seq[String]): String = {
    getMiddleElementSumForOrderedPageLists(lines, false).toString
  }

  private def getMiddleElementSumForOrderedPageLists(lines: Seq[String], forThoseInRightOrder: Boolean): Int = {
    val (orderingRules, updatePageNumbersSeq) = getOrderingRulesAndUpdatePageNumbers(lines)

    val nodes: mutable.Map[String, DirectedGraphNode] = mutable.Map.empty

    orderingRules.foreach { case (first, second) =>
      val firstNode = nodes.getOrElseUpdate(first, DirectedGraphNode(first))
      val secondNode = nodes.getOrElseUpdate(second, DirectedGraphNode(second))
      firstNode.connectTo(secondNode)
    }

    val nodeGraph = new DirectedGraph(nodes.values.toList)

    val orderedLines = updatePageNumbersSeq.map { updatePageNumbers =>
      val reducedGraph = nodeGraph
        .keepOnlyNodes(
          nodes.filter { case (id, _) => updatePageNumbers.contains(id) }.map { case (_, node) => node }.toSeq)

      val orderedNodeIds = reducedGraph.getNodesInOrder.map(_.id)
      val inOrder = orderedNodeIds == updatePageNumbers
      (inOrder, orderedNodeIds)
    }

    orderedLines.filter { case (wasOrdered, _) => wasOrdered == forThoseInRightOrder }.map { case (_, line) => line }
      .map { line => line((line.size - 1) / 2).toInt }.sum
  }

  private def getOrderingRulesAndUpdatePageNumbers(lines: Seq[String]): (Seq[(String, String)], Seq[Seq[String]]) = {
    val orderingRules = lines.takeWhile(_.nonEmpty).map(_.split("\\|").toSeq)
      .map { case Seq(first: String, second: String) => (first, second) }
    val updatePageNumbersSeq = lines.takeRight(lines.size - orderingRules.size - 1).map(_.split(",").toSeq)
    (orderingRules, updatePageNumbersSeq)
  }
}
