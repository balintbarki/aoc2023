package aoctest.aoc2024test.day7

import aoc.aoc2024.day7.Day7Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay7 extends PuzzleTest(Day7Puzzle) {

  @Test
  def testDay7Part1(): Unit = {
    testPart1("3749")
  }

  @Test
  def testDay7Part1real(): Unit = {
    testPart1Real("975671981569")
  }

  @Test
  def testDay7Part2(): Unit = {
    testPart2("11387")
  }

  @Test
  def testDay7Part2real(): Unit = {
    testPart2Real("223472064194845")
  }
}
