package aoc2023test.day1

import aoc2023.day1._
import org.junit.{Assert, Test}

class TestDay1 {

  val testInputPath = "src/test/scala/aoc2023test/day1/testInput_Day1Part1.txt"

  @Test
  def testCalculateLineResult(): Unit = {
    Assert.assertEquals(29, Day1App.calculateLineResult("two1nine", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(83, Day1App.calculateLineResult("eightwothree", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(13, Day1App.calculateLineResult("abcone2threexyz", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(24, Day1App.calculateLineResult("xtwone3four", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(42, Day1App.calculateLineResult("4nineeightseven2", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(14, Day1App.calculateLineResult("zoneight234", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(76, Day1App.calculateLineResult("7pqrstsixteen", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(18, Day1App.calculateLineResult("1one6gxkknfmc2oneightq", Some(Day1App.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(18, Day1App.calculateLineResult("1oneight", Some(Day1App.replaceFirstAndLastDigitStrings)))
  }

  @Test
  def testDay1Part1_real(): Unit = {
    Assert.assertEquals(54951, Day1_Part1.calculate(Day1App.inputPath))
  }

  @Test
  def testDay1Part1(): Unit = {
    val inputPath = "src/test/scala/aoc2023test/day1/testInput_Day1Part1.txt"
    Assert.assertEquals(142, Day1_Part1.calculate(inputPath))
  }

  @Test
  def testDay1Part2(): Unit = {
    val inputPath = "src/test/scala/aoc2023test/day1/testInput_Day1Part2.txt"
    Assert.assertEquals(281, Day1_Part2.calculate(inputPath))
  }
}
