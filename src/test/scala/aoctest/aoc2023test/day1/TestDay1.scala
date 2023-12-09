package aoctest.aoc2023test.day1

import aoc.aoc2023.day1.Day1Puzzle
import aoctest.PuzzleTest
import org.junit.{Assert, Test}

class TestDay1 extends PuzzleTest(Day1Puzzle) {

  @Test
  def testCalculateLineResult(): Unit = {
    Assert
      .assertEquals(29, Day1Puzzle.calculateLineResult("two1nine", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(83,
      Day1Puzzle.calculateLineResult("eightwothree", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert
      .assertEquals(13,
        Day1Puzzle.calculateLineResult("abcone2threexyz", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert
      .assertEquals(24, Day1Puzzle.calculateLineResult("xtwone3four", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert
      .assertEquals(42,
        Day1Puzzle.calculateLineResult("4nineeightseven2", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert
      .assertEquals(14, Day1Puzzle.calculateLineResult("zoneight234", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(76,
      Day1Puzzle.calculateLineResult("7pqrstsixteen", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert.assertEquals(18,
      Day1Puzzle.calculateLineResult("1one6gxkknfmc2oneightq", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
    Assert
      .assertEquals(18, Day1Puzzle.calculateLineResult("1oneight", Some(Day1Puzzle.replaceFirstAndLastDigitStrings)))
  }

  @Test
  def testDay1Part1_real(): Unit = {
    testPart1Real("54951")
  }

  @Test
  def testDay1Part1(): Unit = {
    testPart1("142", Some(testInputPathPart1))
  }

  @Test
  def testDay1Part2_real(): Unit = {
    testPart2Real("55218")
  }

  @Test
  def testDay1Part2(): Unit = {
    testPart2("281", Some(testInputPathPart2))
  }
}
