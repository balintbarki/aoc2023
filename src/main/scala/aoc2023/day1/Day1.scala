package aoc2023.day1

import aoc2023.DailyApp

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala


object Day1App {

  val inputPath = "src/main/scala/aoc2023/day1/input_Day1.txt"
  private val replaceRegexDigitsAsStr = "one|two|three|four|five|six|seven|eight|nine"
  private val replaceRegex = s"$replaceRegexDigitsAsStr|[0-9]"
  private val replaceRegexReversed = s"${replaceRegexDigitsAsStr.reverse}|[0-9]"
  private val replaceMap = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
  )

  def replaceFirstAndLastDigitStrings(line: String): String = {

    val firstReplaced = replaceRegex.r.findFirstIn(line)
      .map(matched => line.replaceFirst(matched, replaceMap.getOrElse(matched, matched))).getOrElse(line)
    val firstReplacedReversed = firstReplaced.reverse
    val lastReplaced = replaceRegexReversed.r.findFirstIn(firstReplacedReversed).map(
      matched => firstReplacedReversed.replaceFirst(matched, replaceMap.getOrElse(matched.reverse, matched.reverse)))
      .getOrElse(firstReplacedReversed).reverse

    lastReplaced
  }

  def calculateCalibration(lines: Seq[String], preprocessFnOpt: Option[String => String] = None): Int = {
    lines.map(line => calculateLineResult(line, preprocessFnOpt)).sum
  }

  def calculateLineResult(line: String, preprocessFnOpt: Option[String => String] = None): Int = {
    val preprocessedLine = preprocessFnOpt.map(preprocessFn => preprocessFn(line)).getOrElse(line)
    val digits = preprocessedLine.toCharArray.filter(c => c.isDigit)
    val first = digits.head.asDigit
    val last = digits.last.asDigit
    val combined = first * 10 + last
    combined
  }
}

object Day1_Part1 extends DailyApp {

  override def calculate(inputPath: String): Int = {
    Day1App.calculateCalibration(Files.readAllLines(Paths.get(inputPath)).asScala.toSeq)
  }

  println(calculate(Day1App.inputPath))
}

object Day1_Part2 extends DailyApp {

  override def calculate(inputPath: String): Int = {
    val lines = Files.readAllLines(Paths.get(inputPath)).asScala.toSeq
    Day1App.calculateCalibration(lines, Some(Day1App.replaceFirstAndLastDigitStrings))
  }

  println(calculate(Day1App.inputPath))
}
