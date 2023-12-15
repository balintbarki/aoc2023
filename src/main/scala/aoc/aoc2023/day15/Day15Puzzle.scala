package aoc.aoc2023.day15

import aoc.aoc2023.DailyPuzzle2023

case object Day15Puzzle extends DailyPuzzle2023(15, "Lens Library") {

  def hash(s: String): Int = s.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)

  override def calculatePart1(lines: Seq[String]): String = {
    lines.head.split(",").map(hash).sum.toString
  }

  override def calculatePart2(lines: Seq[String]): String = ???

}
