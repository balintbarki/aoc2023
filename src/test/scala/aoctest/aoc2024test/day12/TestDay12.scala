package aoctest.aoc2024test.day12

import aoc.aoc2024.day12.Day12Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay12 extends PuzzleTest(Day12Puzzle) {

  @Test
  def testDay12Part1(): Unit = {
    testPart1("1930")
  }

  @Test
  def testDay12Part1real(): Unit = {
    testPart1Real("1370100")
  }

  @Test
  def testDay12Part2(): Unit = {
    testPart2("1206")
  }

  @Test
  def testDay12Part2real(): Unit = {
    testPart2Real("818286")
  }
}
