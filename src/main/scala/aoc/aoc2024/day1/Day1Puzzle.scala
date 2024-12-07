package aoc.aoc2024.day1

import aoc.aoc2024.DailyPuzzle2024

case object Day1Puzzle extends DailyPuzzle2024(1, "Historian Hysteria") {

  override def calculatePart1(lines: Seq[String]): Long = {

    val (leftList, rightList) = getInput(lines)

    leftList.sorted.zip(rightList.sorted).map { case (left, right) => (left - right).abs }.sum
  }

  override def calculatePart2(lines: Seq[String]): Long = {
    val (leftList, rightList) = getInput(lines)

    leftList.map(number => number * rightList.filter(_ == number).size).sum
  }

  private def getInput(lines: Seq[String]) = lines.map(line => line.split("\\s+").toSeq.map(_.toInt))
    .map(numbers => (numbers.head, numbers(1))).unzip

}
