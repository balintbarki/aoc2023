package aoc.aoc2024.day8

import aoc.aoc2024.DailyPuzzle2024

case object Day8Puzzle extends DailyPuzzle2024(8, "Resonant Collinearity") {

  import VectorOperations._

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, getNodesFromAntennasPart1)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, getNodesFromAntennasPart2)

  private def getNodesFromAntennasPart1(
    first: (Int, Int), second: (Int, Int), maxX: Int, maxY: Int): Seq[(Int, Int)] = {
    val delta = second - first
    Seq(first - delta, second + delta)
  }

  private def getNodesFromAntennasPart2(
    first: (Int, Int), second: (Int, Int), maxX: Int, maxY: Int): Seq[(Int, Int)] = {
    val delta = second - first
    val maxSteps = math.max(maxX / delta._1, maxY / delta._2) + 1
    Range(0, maxSteps).flatMap { idx => Seq(first - delta * idx, first + delta * idx) }
  }

  private def calculate(
    lines: Seq[String], getNodesFromAntennas: ((Int, Int), (Int, Int), Int, Int) => Seq[(Int, Int)]): Long = {

    val nodes = lines.indices.flatMap(rowIdx => lines(rowIdx).indices.flatMap(colIdx => lines(rowIdx)(colIdx) match {
      case c if ('a' to 'z').contains(c) || ('A' to 'Z').contains(c) || ('0' to '9').contains(c) => Some(
        ((colIdx, rowIdx), c))
      case _                                                                                     => None
    }))

    val maxX = lines.head.length - 1
    val maxY = lines.size - 1

    val uniqueChars = nodes.map { case (_, c) => c }.distinct

    val allNewNodes = uniqueChars.flatMap { c =>
      val nodesWithC = nodes.filter { case (_, _c) => _c == c }.map { case (coordinates, _) => coordinates }

      val allUniquePairsForC = nodesWithC.indices.dropRight(1)
        .flatMap { idx => Range(idx + 1, nodesWithC.size).map(otherIdx => (nodesWithC(idx), nodesWithC(otherIdx))) }

      val newNodes = allUniquePairsForC.flatMap { case (first, second) =>
        getNodesFromAntennas(first, second, maxX, maxY)
      }.filter { case (x, y) => ((0 <= x) && (x <= maxX) && (0 <= y) && (y <= maxY)) }

      newNodes
    }

    allNewNodes.distinct.size
  }

  object VectorOperations {
    implicit class VectorOperation(vector: (Int, Int)) {
      def +(other: (Int, Int)): (Int, Int) = (vector._1 + other._1, vector._2 + other._2)

      def -(other: (Int, Int)): (Int, Int) = (vector._1 - other._1, vector._2 - other._2)

      def *(factor: Int): (Int, Int) = (vector._1 * factor, vector._2 * factor)
    }
  }
}
