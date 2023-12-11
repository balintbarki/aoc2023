package aoctest.aoc2023test.day11

import aoc.aoc2023.day11.Day11Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay11 extends PuzzleTest(Day11Puzzle) {

  @Test
  def testDay11Part1(): Unit = {
    testPart1("374")
  }

  @Test
  def testDay11Part1real(): Unit = {
    testPart1Real("9965032")
  }

  @Test
  def testDay11Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay11Part2real(): Unit = {
    testPart2Real("0")
  }
}
