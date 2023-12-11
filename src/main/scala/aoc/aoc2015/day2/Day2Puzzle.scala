package aoc.aoc2015.day2

import aoc.aoc2015.DailyPuzzle2015

case object Day2Puzzle extends DailyPuzzle2015(2, "I Was Told There Would Be No Math") {

  override def calculatePart1(
    lines: Seq[String]): String = {

    getInput(lines).map(sides => {
      for {
        (first, firstIdx) <- sides.zipWithIndex
        (second, secondIdx) <- sides.zipWithIndex
        if firstIdx < secondIdx
      } yield first * second * 2
    }.sum + sides.head * sides(1)
    ).sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = getInput(lines).map(sides => (sides.head + sides(1)) * 2 + sides.product).sum.toString

  private def getInput(lines: Seq[String]): Seq[Seq[Int]] = lines.map(line => {
    line.split("x").map(_.toInt).toSeq.sorted
  })
}
