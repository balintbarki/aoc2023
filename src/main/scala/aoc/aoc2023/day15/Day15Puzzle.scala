package aoc.aoc2023.day15

import aoc.aoc2023.DailyPuzzle2023

import scala.collection.mutable

case object Day15Puzzle extends DailyPuzzle2023(15, "Lens Library") {

  override def calculatePart1(lines: Seq[String]): Long = lines.head.split(",").map(hash).sum

  override def calculatePart2(lines: Seq[String]): Long = {

    val removeRegex = """(\w+)-""".r
    val insertRegex = """(\w+)=(\d)""".r
    val boxes: Seq[Box] = (0 to 255).map(Box)

    lines.head.split(",").foreach {
      case removeRegex(label)        => boxes(hash(label)).lensSlots.remove(label)
      case insertRegex(label, focus) => boxes(hash(label)).lensSlots.update(label, focus.toInt)
    }

    boxes
      .map(box => box.lensSlots.zipWithIndex.map { case ((_, focus), index) => (box.id + 1) * (index + 1) * focus }.sum)
      .sum
  }

  private def hash(s: String): Int = s.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)

  private case class Box(id: Int) {
    val lensSlots: mutable.LinkedHashMap[String, Int] = mutable.LinkedHashMap()
  }
}
