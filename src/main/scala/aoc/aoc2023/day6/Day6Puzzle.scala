package aoc.aoc2023.day6

import aoc.aoc2023.DailyPuzzle2023

case object Day6Puzzle extends DailyPuzzle2023(6, "Wait For It") {
  override def calculatePart2(
    lines: Seq[String]): Long = calculatePart1(lines.map(line => line.replaceAll("\\s", "")))

  override def calculatePart1(
    lines: Seq[String]): Long = {
    val times = lines.head.split(":").last.trim.split("\\s+").map(_.toLong)
    val records = lines.tail.head.split(":").last.trim.split("\\s+").map(_.toLong)
    require(times.length == records.length)

    val inputs = times.zip(records)

    calculate(inputs)
  }

  private def calculate(inputs: Seq[(Long, Long)]): Int = {
    inputs.map { case (time, record) => (0L to time).count(pushTime => {
      pushTime * (time - pushTime) > record
    })
    }.product
  }
}
