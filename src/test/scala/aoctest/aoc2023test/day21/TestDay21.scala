package aoctest.aoc2023test.day21

import aoc.aoc2023.day21.Day21Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay21 extends PuzzleTest(Day21Puzzle) {

  @Test
  def testDay21Part1(): Unit = {
    Day21Puzzle.stepCnt = 6
    testPart1("16")
  }

  @Test
  def testDay21Part1real(): Unit = {
    Day21Puzzle.stepCnt = 64
    testPart1Real("3737")
  }

  @Ignore
  @Test
  def testDay21Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay21Part2real(): Unit = {
    testPart2Real("0")
  }
}
