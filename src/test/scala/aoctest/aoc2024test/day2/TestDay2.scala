package aoctest.aoc2024test.day2

import aoc.aoc2024.day2.Day2Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay2 extends PuzzleTest(Day2Puzzle) {

  @Test
  def testDay2Part1(): Unit = {
    testPart1("2")
  }

  @Test
  def testDay2Part1real(): Unit = {
    testPart1Real("334")
  }

  @Test
  def testDay2Part2(): Unit = {
    testPart2("4")
  }

  @Test
  def testDay2Part2real(): Unit = {
    testPart2Real("400")
  }
}
