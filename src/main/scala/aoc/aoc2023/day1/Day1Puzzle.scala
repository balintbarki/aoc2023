package aoc.aoc2023.day1

import aoc.DailyPuzzle


case object Day1Puzzle extends DailyPuzzle(1, "Trebuchet?!") {
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

  override def calculatePart1(
    lines: Seq[String]): String = calculateCalibration(lines).toString

  def calculateLineResult(line: String, preprocessFnOpt: Option[String => String] = None): Int = {
    val preprocessedLine = preprocessFnOpt.map(preprocessFn => preprocessFn(line)).getOrElse(line)
    val digits = preprocessedLine.toCharArray.filter(c => c.isDigit)
    val first = digits.head.asDigit
    val last = digits.last.asDigit
    val combined = first * 10 + last
    combined
  }

  override def calculatePart2(
    lines: Seq[String]): String = calculateCalibration(lines, Some(replaceFirstAndLastDigitStrings)).toString

  def replaceFirstAndLastDigitStrings(line: String): String = {

    val firstReplaced = replaceRegex.r.findFirstIn(line)
      .map(matched => line.replaceFirst(matched, replaceMap.getOrElse(matched, matched))).getOrElse(line)
    val firstReplacedReversed = firstReplaced.reverse
    val lastReplaced = replaceRegexReversed.r.findFirstIn(firstReplacedReversed).map(
      matched => firstReplacedReversed.replaceFirst(matched, replaceMap.getOrElse(matched.reverse, matched.reverse)))
      .getOrElse(firstReplacedReversed).reverse

    lastReplaced
  }

  private def calculateCalibration(lines: Seq[String], preprocessFnOpt: Option[String => String] = None): Int = {
    lines.map(line => calculateLineResult(line, preprocessFnOpt)).sum
  }
}
