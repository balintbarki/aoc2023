package aoctest.aoc2023test.day16

import aoc.aoc2023.day16.Day16Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay16 extends PuzzleTest(Day16Puzzle) {

  @Test
  def testDay16Part1(): Unit = {
    testPart1("46")
  }

  @Test
  def testDay16Part1real(): Unit = {
    testPart1Real("7074")
  }

  @Test
  def testDay16Part2(): Unit = {
    testPart2("51")
  }

  @Test
  def testDay16Part2real(): Unit = {
    testPart2Real("7530")
  }
}
