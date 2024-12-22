package aoctest.aoc2023test.day24

import aoc.aoc2023.day24.Day24Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay24 extends PuzzleTest(Day24Puzzle) {

  @Test
  def testDay24Part1(): Unit = {
    testPart1("2")
  }

  @Test
  def testDay24Part1real(): Unit = {
    Day24Puzzle.minPos = BigDecimal(200000000000000L)
    Day24Puzzle.maxPos = BigDecimal(400000000000000L)
    testPart1Real("11098")
  }

  @Ignore
  @Test
  def testDay24Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay24Part2real(): Unit = {
    testPart2Real("0")
  }
}
