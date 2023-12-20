package aoctest.aoc2023test.day19

import aoc.aoc2023.day19.Day19Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay19 extends PuzzleTest(Day19Puzzle) {

  @Test
  def testDay19Part1(): Unit = {
    testPart1("19114")
  }

  @Test
  def testDay19Part1real(): Unit = {
    testPart1Real("456651")
  }

  @Test
  def testDay19Part2(): Unit = {
    testPart2("167409079868000")
  }

  @Test
  def testDay19Part2real(): Unit = {
    testPart2Real("131899818301477")
  }
}
