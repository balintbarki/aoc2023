package aoctest.aoc2015test.day1

import aoc.aoc2015.day1.Day1Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay1 extends PuzzleTest(Day1Puzzle) {

  @Test
  def testDay1Part1real(): Unit = {
    testPart1Real("280")
  }

  @Test
  def testDay1Part2real(): Unit = {
    testPart2Real("1797")
  }
}
