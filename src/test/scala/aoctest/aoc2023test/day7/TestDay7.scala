package aoctest.aoc2023test.day7

import aoc.aoc2023.day7.Day7Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay7 extends PuzzleTest(Day7Puzzle) {

  @Test
  def testDay7Part1(): Unit = {
    testPart1("6440")
  }

  @Test
  def testDay7Part1real(): Unit = {
    testPart1Real("248569531")
  }

  @Test
  def testDay7Part2(): Unit = {
    testPart2("5905")
  }

  @Test
  def testDay7Part2real(): Unit = {
    testPart2Real("250382098")
  }
}
