package aoc.aoc2015.day1

import aoc.aoc2015.DailyPuzzle2015

case object Day1Puzzle extends DailyPuzzle2015(1, "Not Quite Lisp") {
  override def calculatePart1(
    lines: Seq[String]): Long = (lines.head.count(_ == '(') - lines.head.count(_ == ')'))

  override def calculatePart2(
    lines: Seq[String]): Long = lines.head.foldLeft(Seq[Int](0))((positions, c) => positions :+ {
    if (c == '(') (positions.last + 1) else (positions.last - 1)
  }).indexWhere(_ < 0)
}
