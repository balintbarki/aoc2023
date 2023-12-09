package aoctest.aoc2023test.day4

import aoc.aoc2023.day4.Day4Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay4 extends PuzzleTest(Day4Puzzle) {

  @Test
  def testDay4Part1_real(): Unit = {
    testPart1Real("19135")
  }

  @Test
  def testDay4Part2_real(): Unit = {
    testPart2Real("5704953")
  }

  @Test
  def testDay4Part1(): Unit = {
    testPart1("13")
  }

  @Test
  def testDay4Part2(): Unit = {
    testPart2("30")
  }
}
