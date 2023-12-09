package aoc.aoc2023.day5

import aoc.DailyPuzzle
import aoc.aoc2023.day5

import scala.annotation.tailrec

case object Day5Puzzle extends DailyPuzzle(5, "If You Give A Seed A Fertilizer") {
  // Stolen from https://stackoverflow.com/a/21803339
  implicit class AddMultispanToList[A](val list: List[A]) extends AnyVal {
    def multiSpan(splitOn: A => Boolean): List[List[A]] = {
      @tailrec
      def loop(xs: List[A], acc: List[List[A]]): List[List[A]] = xs match {
        case Nil      => acc
        case x :: Nil => List(x) :: acc
        case h :: t   =>
          val (pre, post) = t.span(!splitOn(_))
          loop(post, (h :: pre) :: acc)
      }

      loop(list, Nil).reverse
    }
  }

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
