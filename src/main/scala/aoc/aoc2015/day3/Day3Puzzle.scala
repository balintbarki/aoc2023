package aoc.aoc2015.day3

import aoc.aoc2015.DailyPuzzle2015

case object Day3Puzzle extends DailyPuzzle2015(3, "Perfectly Spherical Houses in a Vacuum") {

  override def calculatePart1(lines: Seq[String]): Long = processSteps(lines.head).distinct.size

  override def calculatePart2(
    lines: Seq[String]): Long = {
    val indexedChars = lines.head.zipWithIndex
    (0 to 1).flatMap(i => processSteps(indexedChars.collect { case (c, index) if (index % 2 == i) => c })).distinct.size

  }

  private def processSteps(line: IndexedSeq[Char]): Seq[House] = line
    .foldLeft(Seq(House(0, 0)))((houses, direction) => {
      val lastHouse = houses.last
      houses :+ {
        direction match {
          case '^' => House(lastHouse.x, lastHouse.y + 1)
          case 'v' => House(lastHouse.x, lastHouse.y - 1)
          case '>' => House(lastHouse.x + 1, lastHouse.y)
          case '<' => House(lastHouse.x - 1, lastHouse.y)
        }
      }
    })

  private case class House(x: Long, y: Long)
}
