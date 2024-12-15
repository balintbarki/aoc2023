package aoc.aoc2024.day11

import aoc.aoc2024.DailyPuzzle2024

case object Day11Puzzle extends DailyPuzzle2024(11, "Plutonian Pebbles") {

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, 25)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, 75)

  private def calculate(lines: Seq[String], blinkCnt: Int): Long = {
    val numbers = lines.head.split(" ").map(_.toLong).toSeq.map(number => (number, 1.toLong))

    val blinkedNumbers = (0 until blinkCnt).foldLeft(numbers)((prevNumbers, _) => {
      //println(s"Unique/total numbers in prevNumbers: ${prevNumbers.distinct.size}/${prevNumbers.size}")
      blink(prevNumbers)
    })
    blinkedNumbers.map(_._2).sum
  }

  private def blink(numbers: Seq[(Long, Long)]): Seq[(Long, Long)] = {
    val stonesAfterBlink = numbers.flatMap { case (number, cnt) =>
      number match {
        case 0                                 => Seq((1.toLong, cnt))
        case n if (n.toString.length % 2) == 0 =>
          val nString = n.toString
          val halfLength = nString.length / 2
          Seq((nString.take(halfLength).toLong, cnt), (nString.takeRight(halfLength).toLong, cnt))
        case other                             => Seq((other * 2024, cnt))
      }
    }

    val uniqueNumbers = stonesAfterBlink.map(_._1).distinct
    uniqueNumbers.map(number => (number, stonesAfterBlink.filter(_._1 == number).map(_._2).sum))
  }
}
