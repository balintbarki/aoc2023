package aoc.aoc2024.day11

import aoc.aoc2024.DailyPuzzle2024

case object Day11Puzzle extends DailyPuzzle2024(11, "Plutonian Pebbles") {

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, 25)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, 75)

  private def calculate(lines: Seq[String], blinkCnt: Int): Long = {
    val numbers = lines.head.split(" ").map(_.toLong).toSeq

    val blinkedNumbers = (0 until blinkCnt).foldLeft(numbers)((prevNumbers, cnt) => {
      blink(prevNumbers)
    })
    blinkedNumbers.size
  }

  private def blink(numbers: Seq[Long]): Seq[Long] = {
    numbers.flatMap { number =>
      number match {
        case 0                                 => Seq(1)
        case n if (n.toString.length % 2) == 0 =>
          val nString = n.toString
          val halfLength = nString.length / 2
          Seq(nString.take(halfLength).toInt, nString.takeRight(halfLength).toInt)
        case other                             => Seq(other * 2024)
      }
    }
  }
}
