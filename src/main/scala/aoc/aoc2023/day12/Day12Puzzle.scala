package aoc.aoc2023.day12

import aoc.aoc2023.DailyPuzzle2023

import scala.collection.mutable

case object Day12Puzzle extends DailyPuzzle2023(12, "Hot Springs") {

  val operationalCode = "O"
  val damagedCode = "X"
  val unknownCode = "_"

  override def calculatePart1(lines: Seq[String]): Long = calculateUnfolded(lines, 1)

  override def calculatePart2(lines: Seq[String]): Long = calculateUnfolded(lines, 5)

  private def calculateUnfolded(lines: Seq[String], unfoldCnt: Int): Long = {

    // Memoization cache
    val memo: mutable.Map[(String, Long, Seq[Int]), Long] = mutable.Map()

    val input = lines.map(line => {
      val parts = line.split(" ")
      val record = Seq.fill(unfoldCnt)(parts.head.trim).mkString("?")
        .replaceAll("\\.", operationalCode)
        .replaceAll("#", damagedCode)
        .replaceAll("\\?", unknownCode)

      val groups = Seq.fill(unfoldCnt)(parts.last).mkString(",")
        .trim.split(",")
        .map(_.toInt).toSeq

      (record, groups)
    })

    def calculate(line: String, acc: Long, groups: Seq[Int]): Long = {

      memo.getOrElseUpdate((line, acc, groups), {

        val minLength = groups.sum + groups.length - 1

        val result = if (groups.isEmpty) {
          if (line.contains(damagedCode))
          // This path is wrong, no groups left but there are damaged mirrors in the input
            0L
          else
            1
        }
        else if (line.length < minLength)
        // This path is wrong, no input left but there are groups to represent
          0L
        else {
          val separator = if (groups.length > 1)
          // Separator before the next group
            s"[$unknownCode$operationalCode]"
          else
            ""

          val startsWithDamagedGroupRegex = s"^([$damagedCode$unknownCode]{${groups.head}}$separator)".r.unanchored
          val startsWithUnknownRegex = s"^$unknownCode".r.unanchored
          val startsWithOperationalRegex = s"^($operationalCode+)".r.unanchored

          val startsWithGroupResult = startsWithDamagedGroupRegex.findFirstMatchIn(line) match {
            case Some(matcher) => calculate(line.drop(matcher.group(1).length), acc, groups.drop(1))
            case None          => 0
          }

          val startsWithUnknownResult = startsWithUnknownRegex.findFirstMatchIn(line) match {
            case Some(_) => calculate(line.drop(1), acc, groups)
            case None    => 0
          }

          val startsWithOperationalResult = startsWithOperationalRegex.findFirstMatchIn(line) match {
            case Some(matcher) => calculate(line.drop(matcher.group(1).length), acc, groups)
            case None          => 0
          }

          acc + (startsWithGroupResult + startsWithUnknownResult + startsWithOperationalResult)
        }

        result
      })
    }

    input.map { case (record, groups) => calculate(record, 0L, groups) }.sum
  }
}
