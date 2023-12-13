package aoc.aoc2023.day13

import aoc.aoc2023.DailyPuzzle2023

import scala.math.Ordered.orderingToOrdered

case object Day13Puzzle extends DailyPuzzle2023(13, "Point of Incidence") {

  import aoc.utils.ImplicitUtils._

  override def calculatePart1(
    lines: Seq[String]): String = {

    val inputs = getInput(lines).map(_.filter(_.nonEmpty))

    inputs.map { input =>
      val rowHashes = input.map(_.hashCode()).zipWithIndex
      val columnHashes = input.head.indices.map(index => input.map(row => row(index)).mkString.hashCode).toList
        .zipWithIndex
      val rowReflectionIndex = findReflection(rowHashes)
      val columnReflectionIndex = findReflection(columnHashes)
      val result = columnReflectionIndex + 100 * rowReflectionIndex
      //println(s"rowReflectionIndex: $rowReflectionIndex, columnReflectionIndex: $columnReflectionIndex")
      if (result == 0) {
        input.foreach(println)
        ???
      }
      result
    }.sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = ???

  private def findReflection(inputs: List[(Int, Int)]): Int = {

    val inputHashes = inputs.map { case (hash, _) => hash }
    val inputCnt = inputs.length
    val mirrorLineOpt = (1 until inputHashes.length).find(index => {
      val takeCnt = math.min(index, inputCnt - index)
      val (first, second) = inputHashes.splitAt(index)
      first.takeRight(takeCnt) == second.take(takeCnt).reverse
    })

    mirrorLineOpt match {
      case Some(mirrorLine) => mirrorLine
      case _                => 0
    }
  }

  private def getInput(lines: Seq[String]) = {
    lines.toList.multiSpan(_.isEmpty)
  }

}
