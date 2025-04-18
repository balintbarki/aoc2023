package aoctest.aoc2024test.day10

import aoc.aoc2024.day10.Day10Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay10 extends PuzzleTest(Day10Puzzle) {

  @Test
  def testDay10Part1(): Unit = {
    testPart1("36")
  }

  @Test
  def testDay10Part1real(): Unit = {
    testPart1Real("754")
  }

  @Test
  def testDay10Part2(): Unit = {
    testPart2("81")
  }

  @Test
  def testDay10Part2real(): Unit = {
    testPart2Real("1609")
  }
}
