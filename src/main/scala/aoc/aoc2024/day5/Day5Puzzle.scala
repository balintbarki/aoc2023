package aoc.aoc2024.day5

import aoc.aoc2024.DailyPuzzle2024

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case object Day5Puzzle extends DailyPuzzle2024(5, "Print Queue") {

  override def calculatePart1(lines: Seq[String]): String = {

    val orderingRules = lines.takeWhile(_.nonEmpty).map(_.split("\\|").toSeq)
    val updatePageNumbersSeq = lines.takeRight(lines.size - orderingRules.size - 1).map(_.split(",").toSeq)

    val mustPrintLaterByPages: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty
    val mustPrintEarlierByPages: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty

    orderingRules.foreach { case Seq(first, second) =>
      val mustPrintLaterForFirst = mustPrintLaterByPages.getOrElseUpdate(first, mutable.Set.empty[String])
      mustPrintLaterForFirst.addOne(second)

      val mustPrintEarlierForSecond = mustPrintEarlierByPages.getOrElseUpdate(second, mutable.Set.empty[String])
      mustPrintEarlierForSecond.addOne(first)
    }

    updatePageNumbersSeq.map { updatePageNumbers =>

      if (fulfilsRules(updatePageNumbers, mustPrintLaterByPages, mustPrintEarlierByPages)) {
        updatePageNumbers((updatePageNumbers.size - 1) / 2).toInt
      } else
        0
    }.sum.toString
  }

  override def calculatePart2(lines: Seq[String]): String = ???


  private def fulfilsRules(
    updatePageNumbers: Seq[String],
    mustPrintLaterByPages: mutable.Map[String, mutable.Set[String]],
    mustPrintEarlierByPages: mutable.Map[String, mutable.Set[String]]): Boolean = {
    updatePageNumbers.indices.forall { leftIndex =>
      if (leftIndex < updatePageNumbers.size - 1) {
        val left = updatePageNumbers(leftIndex)
        Range(leftIndex + 1, updatePageNumbers.size).forall { rightIndex =>
          val right = updatePageNumbers(rightIndex)

          val leftCheck = mustPrintLaterByPages.get(left).forall { mustPrintLater => mustPrintLater.contains(right) }
          val rightCheck = mustPrintEarlierByPages.get(right)
            .forall { mustPrintEarlier => mustPrintEarlier.contains(left) }

          val result = leftCheck && rightCheck
          result
        }
      }
      else
        true

    }
  }
}
