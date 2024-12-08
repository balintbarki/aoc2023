package aoctest.aoc2024test.day8

import aoc.aoc2024.day8.Day8Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay8 extends PuzzleTest(Day8Puzzle) {

  @Test
  def testDay8Part1(): Unit = {
    testPart1("14")
  }

  @Test
  def testDay8Part1real(): Unit = {
    testPart1Real("396")
  }

  @Test
  def testDay8Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay8Part2real(): Unit = {
    testPart2Real("0")
  }
}
