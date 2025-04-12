package aoctest.aoc2024test.day17

import aoc.aoc2024.day17.Day17Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay17 extends PuzzleTest(Day17Puzzle) {

  @Test
  def testDay17Part1(): Unit = {
    testPart1("4635635210", Some(testInputPathPart1))
  }

  @Test
  def testDay17Part1real(): Unit = {
    testPart1Real("150174103")
  }

  @Test
  def testDay17Part2(): Unit = {
    testPart2("117440", Some(testInputPathPart2))
  }

  @Test
  def testDay17Part2real(): Unit = {
    testPart2Real("0")
  }
}
