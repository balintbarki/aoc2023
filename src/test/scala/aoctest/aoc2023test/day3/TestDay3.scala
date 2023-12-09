package aoctest.aoc2023test.day3

import aoc.aoc2023.day3.Day3Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay3 extends PuzzleTest(Day3Puzzle) {

  @Test
  def testDay3Part1_real(): Unit = {
    testPart1Real("554003")
  }

  @Test
  def testDay3Part1(): Unit = {
    testPart1("4361", inputPath = Some(testInputPath))
  }

  @Test
  def testDay3Part2(): Unit = {
    testPart2("467835", inputPath = Some(testInputPath))
  }
}
