package aoctest.aoc2024test.day14

import aoc.aoc2024.day14.Day14Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay14 extends PuzzleTest(Day14Puzzle) {

  @Test
  def testDay14Part1(): Unit = {
    testPart1("12")
  }

  @Test
  def testDay14Part1real(): Unit = {
    testPart1Real("221142636")
  }

  @Ignore
  @Test
  def testDay14Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay14Part2real(): Unit = {
    testPart2Real("7916")
  }
}
