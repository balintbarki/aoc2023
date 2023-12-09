package aoctest.aoc2023test.day9

import aoc.aoc2023.day9.Day9Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay9 extends PuzzleTest(Day9Puzzle) {

  @Test
  def testDay9Part1(): Unit = {
    testPart1("114")
  }

  @Test
  def testDay9Part1real(): Unit = {
    testPart1Real("1992273652")
  }

  @Test
  def testDay9Part2(): Unit = {
    testPart2("2")
  }

  @Test
  def testDay9Part2real(): Unit = {
    testPart2Real("1012")
  }
}
