package aoctest.aoc2015test.day2

import aoc.aoc2015.day2.Day2Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay2 extends PuzzleTest(Day2Puzzle) {

  @Test
  def testDay2Part1real(): Unit = {
    testPart1Real("1606483")
  }

  @Test
  def testDay2Part2real(): Unit = {
    testPart2Real("3842356")
  }
}
