package aoctest.aoc2023test.day6

import aoc.aoc2023.day6.Day6Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay6 extends PuzzleTest(Day6Puzzle) {

  @Test
  def testDay6Part1(): Unit = {
    testPart1("288")
  }

  @Test
  def testDay6Part2(): Unit = {
    testPart2("71503")
  }

  @Test
  def testDay6Part1_real(): Unit = {
    testPart1Real("160816")
  }

  @Test
  def testDay6Part2_real(): Unit = {
    testPart2Real("46561107")
  }
}
