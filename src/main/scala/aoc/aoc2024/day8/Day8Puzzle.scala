package aoc.aoc2024.day8

import aoc.aoc2024.DailyPuzzle2024

case object Day8Puzzle extends DailyPuzzle2024(8, "unknown") {

  override def calculatePart1(lines: Seq[String]): Long = {
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

      val newNodes = allUniquePairsForC.flatMap { case ((firstX, firstY), (secondX, secondY)) =>

        def newNodeIfInRange(x: Int, y: Int): Option[(Int, Int)] =
          if ((0 <= x) && (x <= maxX) && (0 <= y) && (y <= maxY))
            Some((x, y))
          else
            None

        val deltaX = math.abs(firstX - secondX)
        val deltaY = math.abs(firstY - secondY)

        val ((firstNewX, firstNewY), (secondNewX, secondNewY)) = if ((firstX <= secondX)) {
          if (firstY <= secondY) {
            // f .
            // . s
            val firstNewX = firstX - deltaX
            val firstNewY = firstY - deltaY
            val secondNewX = secondX + deltaX
            val secondNewY = secondY + deltaY
            ((firstNewX, firstNewY), (secondNewX, secondNewY))
          } else {
            // . s
            // f .
            val firstNewX = firstX - deltaX
            val firstNewY = firstY + deltaY
            val secondNewX = secondX + deltaX
            val secondNewY = secondY - deltaY
            ((firstNewX, firstNewY), (secondNewX, secondNewY))
          }
        } else {
          if (firstY <= secondY) {
            // . f
            // s .
            val firstNewX = firstX + deltaX
            val firstNewY = firstY - deltaY
            val secondNewX = secondX - deltaX
            val secondNewY = secondY + deltaY
            ((firstNewX, firstNewY), (secondNewX, secondNewY))
          } else {
            // s .
            // . f
            val firstNewX = firstX + deltaX
            val firstNewY = firstY + deltaY
            val secondNewX = secondX - deltaX
            val secondNewY = secondY - deltaY
            ((firstNewX, firstNewY), (secondNewX, secondNewY))
          }
        }

        val firstNewNode = newNodeIfInRange(firstNewX, firstNewY)
        val secondNewNode = newNodeIfInRange(secondNewX, secondNewY)

        Seq(firstNewNode, secondNewNode).flatten
      }

      newNodes

    }

    allNewNodes.distinct.size
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

}
