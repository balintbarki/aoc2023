package aoc.aoc2015.day1

import aoc.aoc2015.DailyPuzzle2015

case object Day1Puzzle extends DailyPuzzle2015(1, "Not Quite Lisp") {
  override def calculatePart1(
    lines: Seq[String]): String = (lines.head.count(_ == '(') - lines.head.count(_ == ')')).toString

  override def calculatePart2(
    lines: Seq[String]): String = lines.head.foldLeft(Seq[Int](0))((positions, c) => positions :+ {
    if (c == '(') (positions.last + 1) else (positions.last - 1)
  }).indexWhere(_ < 0).toString
}
