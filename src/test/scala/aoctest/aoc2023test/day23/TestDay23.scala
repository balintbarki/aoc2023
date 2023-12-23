package aoctest.aoc2023test.day23

import aoc.aoc2023.day23.Day23Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay23 extends PuzzleTest(Day23Puzzle) {

  @Test
  def testDay23Part1(): Unit = {
    testPart1("94")
  }

  @Test
  def testDay23Part1real(): Unit = {
    testPart1Real("2094")
  }

  @Ignore
  @Test
  def testDay23Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay23Part2real(): Unit = {
    testPart2Real("0")
  }
}
