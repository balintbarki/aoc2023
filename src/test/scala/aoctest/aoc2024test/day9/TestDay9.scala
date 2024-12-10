package aoctest.aoc2024test.day9

import aoc.aoc2024.day9.Day9Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay9 extends PuzzleTest(Day9Puzzle) {

  @Test
  def testDay9Part1(): Unit = {
    testPart1("1928")
  }

  @Test
  def testDay9Part1real(): Unit = {
    testPart1Real("6359213660505")
  }

  @Test
  def testDay9Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay9Part2real(): Unit = {
    testPart2Real("0")
  }
}
