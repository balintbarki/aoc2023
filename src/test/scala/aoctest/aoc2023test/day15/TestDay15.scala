package aoctest.aoc2023test.day15

import aoc.aoc2023.day15.Day15Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay15 extends PuzzleTest(Day15Puzzle) {

  @Test
  def testDay15Part1(): Unit = {
    testPart1("1320")
  }

  @Test
  def testDay15Part1real(): Unit = {
    testPart1Real("513172")
  }

  @Ignore
  @Test
  def testDay15Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay15Part2real(): Unit = {
    testPart2Real("0")
  }
}
