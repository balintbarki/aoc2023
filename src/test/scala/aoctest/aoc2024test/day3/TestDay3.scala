package aoctest.aoc2024test.day3

import aoc.aoc2024.day3.Day3Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay3 extends PuzzleTest(Day3Puzzle) {

  @Test
  def testDay3Part1(): Unit = {
    testPart1("161", Some(testInputPathPart1))
  }

  @Test
  def testDay3Part1real(): Unit = {
    testPart1Real("182619815")
  }

  @Test
  def testDay3Part2(): Unit = {
    testPart2("48", Some(testInputPathPart2))
  }

  @Test
  def testDay3Part2real(): Unit = {
    testPart2Real("80747545")
  }
}
