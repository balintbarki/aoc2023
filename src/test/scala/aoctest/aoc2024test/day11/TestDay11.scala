package aoctest.aoc2024test.day11

import aoc.aoc2024.day11.Day11Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay11 extends PuzzleTest(Day11Puzzle) {

  @Test
  def testDay11Part1(): Unit = {
    testPart1("55312")
  }

  @Test
  def testDay11Part1real(): Unit = {
    testPart1Real("183620")
  }

  @Test
  def testDay11Part2(): Unit = {
    testPart2("65601038650482")
  }

  @Test
  def testDay11Part2real(): Unit = {
    testPart2Real("220377651399268")
  }
}
