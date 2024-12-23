package aoctest.aoc2024test.day16

import aoc.aoc2024.day16.Day16Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay16 extends PuzzleTest(Day16Puzzle) {

  @Test
  def testDay16Part1(): Unit = {
    testPart1("7036")
  }

  @Test
  def testDay16Part1real(): Unit = {
    testPart1Real("83432")
  }

  @Test
  def testDay16Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay16Part2real(): Unit = {
    testPart2Real("0")
  }
}
