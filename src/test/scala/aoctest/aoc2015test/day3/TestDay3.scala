package aoctest.aoc2015test.day3

import aoc.aoc2015.day3.Day3Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay3 extends PuzzleTest(Day3Puzzle) {

  @Test
  def testDay3Part1real(): Unit = {
    testPart1Real("2565")
  }

  @Test
  def testDay3Part2real(): Unit = {
    testPart2Real("2639")
  }
}
