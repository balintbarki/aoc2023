package aoc.aoc2023.day5

import aoc.aoc2023.{DailyPuzzle2023, day5}

import scala.annotation.tailrec

case object Day5Puzzle extends DailyPuzzle2023(5, "If You Give A Seed A Fertilizer") {

  import aoc.utils.ImplicitUtils._

  override def calculatePart1(
    lines: Seq[String]): String = {
    def seedParser: String => Seq[Range] = line =>
      """(\d+)""".r.findAllIn(line).matchData.map(matcher => Range(matcher.matched.toLong)).toSeq

    val (seedRanges, propertyMaps) = parseData(lines, seedParser)

    seedRanges.map(
      seedRange => propertyMaps.foldLeft(seedRange)((range, propertyMap) => Range(propertyMap.shift(range.start))))
      .map(_.start).min.toString
  }

  override def calculatePart2(lines: Seq[String]): String = {

    def seedParser: String => Seq[Range] = line => {
      val pairs =
        """(\d+\s+\d+)""".r.findAllIn(line).matchData
          .map(_.matched.split("\\s").toSeq).toSeq

      val result = pairs.foldLeft(Seq[Range]())(
        (acc, current) => {
          val start = current.head.toLong
          val end = start + current(1).toLong
          acc :+ Range(start, end)
        })

      result
    }

    val (seedRange, propertyMaps) = parseData(lines, seedParser)

    val finalRanges = propertyMaps.foldLeft(seedRange)((ranges, propertyMap) => propertyMap.processRanges(ranges))

    rangesToEndpoints(finalRanges).min.toString
  }

  private def parseData(lines: Seq[String], seedParser: String => Seq[Range]): (Seq[Range], Seq[PropertyMap]) = {
    val numberRegex = """(\d+)""".r
    val seedRanges = seedParser(lines.head)
    val propertyMaps = lines.tail.toList.multiSpan(_.isEmpty)
      .map(lines => lines.filter("\\d+".r.unanchored.matches(_))).map(lines => {
      val rangeShiftDefinitions = lines.map(line => {
        val numbers = numberRegex.findAllIn(line).matchData.map(_.matched.toLong).toSeq
        day5.RangeShiftDefinition(Range(numbers(1), numbers(1) + numbers(2)), numbers.head - numbers(1))
      })
      new PropertyMap(rangeShiftDefinitions)
    })

    (seedRanges, propertyMaps)
  }

  private def rangesToEndpoints(ranges: Seq[Range]): Seq[Long] = ranges
    .flatMap { case Range(start, end) => Seq(start) ++ Seq(end) }.distinct.sorted
}
