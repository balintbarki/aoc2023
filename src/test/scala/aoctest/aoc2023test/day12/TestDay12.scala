package aoctest.aoc2023test.day12

import aoc.aoc2023.day12.Day12Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay12 extends PuzzleTest(Day12Puzzle) {

  @Test
  def testDay12Part1(): Unit = {
    testPart1("21")
  }

  @Test
  def testDay12Part1real(): Unit = {
    testPart1Real("7694")
  }

  @Test
  def testDay12Part2(): Unit = {
    testPart2("525152")
  }

  @Test
  def testDay12Part2real(): Unit = {
    testPart2Real("5071883216318")
  }
}
