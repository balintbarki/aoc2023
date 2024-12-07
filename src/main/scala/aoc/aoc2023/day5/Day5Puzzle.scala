package aoc.aoc2023.day5

import aoc.aoc2023.{DailyPuzzle2023, day5}
import aoc.utils
import aoc.utils.LongRange

import scala.annotation.tailrec

case object Day5Puzzle extends DailyPuzzle2023(5, "If You Give A Seed A Fertilizer") {

  import aoc.utils.ImplicitUtils._

  override def calculatePart1(
    lines: Seq[String]): Long = {
    def seedParser: String => Seq[utils.LongRange] = line =>
      """(\d+)""".r.findAllIn(line).matchData.map(matcher => LongRange(matcher.matched.toLong)).toSeq

    val (seedRanges, propertyMaps) = parseData(lines, seedParser)

    seedRanges.map(
      seedRange => propertyMaps.foldLeft(seedRange)((range, propertyMap) => LongRange(propertyMap.shift(range.start))))
      .map(_.start).min
  }

  override def calculatePart2(lines: Seq[String]): Long = {

    def seedParser: String => Seq[utils.LongRange] = line => {
      val pairs =
        """(\d+\s+\d+)""".r.findAllIn(line).matchData
          .map(_.matched.split("\\s").toSeq).toSeq

      val result = pairs.foldLeft(Seq[utils.LongRange]())(
        (acc, current) => {
          val start = current.head.toLong
          val end = start + current(1).toLong
          acc :+ LongRange(start, end)
        })

      result
    }

    val (seedRange, propertyMaps) = parseData(lines, seedParser)

    val finalRanges = propertyMaps.foldLeft(seedRange)((ranges, propertyMap) => propertyMap.processRanges(ranges))

    rangesToEndpoints(finalRanges).min
  }

  private def parseData(
    lines: Seq[String], seedParser: String => Seq[utils.LongRange]): (Seq[utils.LongRange], Seq[PropertyMap]) = {
    val numberRegex = """(\d+)""".r
    val seedRanges = seedParser(lines.head)
    val propertyMaps = lines.tail.toList.multiSpan(_.isEmpty)
      .map(lines => lines.filter("\\d+".r.unanchored.matches(_))).map(lines => {
      val rangeShiftDefinitions = lines.map(line => {
        val numbers = numberRegex.findAllIn(line).matchData.map(_.matched.toLong).toSeq
        day5.RangeShiftDefinition(LongRange(numbers(1), numbers(1) + numbers(2)), numbers.head - numbers(1))
      })
      new PropertyMap(rangeShiftDefinitions)
    })

    (seedRanges, propertyMaps)
  }

  private def rangesToEndpoints(ranges: Seq[utils.LongRange]): Seq[Long] = ranges
    .flatMap { case LongRange(start, end) => Seq(start) ++ Seq(end) }.distinct.sorted
}
