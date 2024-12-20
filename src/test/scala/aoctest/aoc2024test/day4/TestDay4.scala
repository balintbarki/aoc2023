package aoctest.aoc2024test.day4

import aoc.aoc2024.day4.Day4Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay4 extends PuzzleTest(Day4Puzzle) {

  @Test
  def testDay4Part1(): Unit = {
    testPart1("18")
  }

  @Test
  def testDay4Part1real(): Unit = {
    testPart1Real("2560")
  }

  @Test
  def testDay4Part2(): Unit = {
    testPart2("9")
  }

  @Test
  def testDay4Part2real(): Unit = {
    testPart2Real("1910")
  }
}
