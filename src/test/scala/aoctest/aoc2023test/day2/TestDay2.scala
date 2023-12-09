package aoctest.aoc2023test.day2

import aoc.aoc2023.day2.Day2Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay2 extends PuzzleTest(Day2Puzzle) {

  @Test
  def testDay2Part1(): Unit = {
    testPart1("8")
  }

  @Test
  def testDay2Part2(): Unit = {
    testPart2("2286")
  }

  @Test
  def testDay2Part1_real(): Unit = {
    testPart1Real("2156")
  }

  @Test
  def testDay2Part2_real(): Unit = {
    testPart2Real("66909")
  }
}
