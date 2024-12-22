package aoctest.aoc2024test.day6

import aoc.aoc2024.day6.Day6Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay6 extends PuzzleTest(Day6Puzzle) {

  @Test
  def testDay6Part1(): Unit = {
    testPart1("41")
  }

  @Test
  def testDay6Part1real(): Unit = {
    testPart1Real("5269")
  }

  @Test
  def testDay6Part2(): Unit = {
    testPart2("6")
  }

  @Test
  def testDay6Part2real(): Unit = {
    testPart2Real("1957")
  }
}
