package aoctest.aoc2023test.day13

import aoc.aoc2023.day13.Day13Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay13 extends PuzzleTest(Day13Puzzle) {

  @Test
  def testDay13Part1(): Unit = {
    testPart1("405")
  }

  @Test
  def testDay13Part1real(): Unit = {
    testPart1Real("30518")
  }

  @Test
  def testDay13Part2(): Unit = {
    testPart2("400")
  }

  @Test
  def testDay13Part2real(): Unit = {
    testPart2Real("36735")
  }
}
