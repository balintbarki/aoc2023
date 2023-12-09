package aoc.aoc2023.day5

import aoc.aoc2023.{DailyApp, day5}

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Day5App {

  val inputPath = "src/main/scala/aoc2023/day5/input_Day5.txt"

  def parseData(lines: Seq[String], seedParser: String => Seq[Range]): (Seq[Range], Seq[PropertyMap]) = {
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
}

object Day5_Part1 extends DailyApp {
  override def calculate(inputPath: String): Long = {
    def seedParser: String => Seq[Range] = line =>
      """(\d+)""".r.findAllIn(line).matchData.map(matcher => Range(matcher.matched.toLong)).toSeq

    val lines = Files.readAllLines(Paths.get(inputPath)).asScala.toSeq
    val (seedRanges, propertyMaps) = Day5App
      .parseData(lines, seedParser)

    seedRanges.map(
      seedRange => propertyMaps.foldLeft(seedRange)((range, propertyMap) => Range(propertyMap.shift(range.start))))
      .map(_.start).min
  }

  println(calculate(Day5App.inputPath))
}

object Day5_Part2 extends DailyApp {

  override def calculate(inputPath: String): Long = {
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

    val lines = Files.readAllLines(Paths.get(inputPath)).asScala.toSeq
    val (seedRange, propertyMaps) = Day5App
      .parseData(lines, seedParser)

    val seedRangesToCheck: Seq[Range] = intersectRangesWithPropertyMap(seedRange, propertyMaps.head).map(_.range)
      .distinct

    println(s"Seed ranges to check: $seedRangesToCheck")

    val finalRanges = propertyMaps.foldLeft(seedRangesToCheck)((ranges, propertyMap) => {
      val newRanges = propertyMap.processRanges(ranges)
      println(s"New rangs $newRanges")
      newRanges
    })

    rangesToEndpoints(finalRanges).min
  }

  def intersectRangesWithPropertyMap(ranges: Seq[Range], propertyMap: PropertyMap): Seq[RangeShiftDefinition] = {
    val result = ranges.flatMap(range => intersectRangeWithPropertyMap(range, propertyMap)).sorted
    println(s"Intersect ${ranges.sorted} with property map ${propertyMap.rangeShiftDefinitions.sorted}")
    println(s"Result    $result")
    result.distinct
  }

  def intersectRangeWithPropertyMap(range: Range, propertyMap: PropertyMap): Seq[RangeShiftDefinition] =
    toDisjunctRangeShiftDefinitions(
      propertyMap.rangeShiftDefinitions.flatMap(
        rangeShiftDefinition => intersectRangeWithRangeShiftDefinition(range, rangeShiftDefinition)))

  def intersectRangeWithRangeShiftDefinition(
    range: Range, rangeShiftDefinition: RangeShiftDefinition): Seq[RangeShiftDefinition] = {

    val result = if ((rangeShiftDefinition.range.end <= range.start) || (range.end <= rangeShiftDefinition.range
      .start)) {
      Seq(day5.RangeShiftDefinition(range, 0))
    }
    else if ((range.start <= rangeShiftDefinition.range.start) && (rangeShiftDefinition.range.end <= range.end)) {
      Seq(
        day5.RangeShiftDefinition(Range(range.start, rangeShiftDefinition.range.start), 0),
        rangeShiftDefinition,
        day5.RangeShiftDefinition(Range(rangeShiftDefinition.range.end, range.end), 0))
    }
    else if ((rangeShiftDefinition.range.start <= range.start) && (range.end <= rangeShiftDefinition.range.end)) {
      Seq(day5.RangeShiftDefinition(Range(range.start, range.end), rangeShiftDefinition.shift))
    } else if ((rangeShiftDefinition.range.start <= range.start) && (rangeShiftDefinition.range.end <= range.end)) {
      Seq(
        day5.RangeShiftDefinition(Range(range.start, rangeShiftDefinition.range.end), rangeShiftDefinition.shift),
        day5.RangeShiftDefinition(Range(rangeShiftDefinition.range.end, range.end), 0)
      )
    } else if ((range.start <= rangeShiftDefinition.range.start) && (range.end <= rangeShiftDefinition.range.end)) {
      Seq(
        day5.RangeShiftDefinition(Range(range.start, rangeShiftDefinition.range.start), 0),
        day5.RangeShiftDefinition(Range(rangeShiftDefinition.range.start, range.end), rangeShiftDefinition.shift)
      )
    } else {
      Seq()
    }

    result.distinct
  }

  def toDisjunctRangeShiftDefinitions(ranges: Seq[RangeShiftDefinition]): Seq[RangeShiftDefinition] = ranges

  def rangesToEndpoints(ranges: Seq[Range]): Seq[Long] = ranges
    .flatMap { case Range(start, end) => Seq(start) ++ Seq(end) }.distinct.sorted

  def rangesFromEndpoints(endpoints: Seq[Long]): Seq[Range] =
    if (endpoints.size < 2)
      Seq(Range(endpoints.head, endpoints.head))
    else
      endpoints.sliding(2).map {
        case Seq(start, end) => Range(start, end)
      }.toSeq

  println(calculate(Day5App.inputPath))
}
