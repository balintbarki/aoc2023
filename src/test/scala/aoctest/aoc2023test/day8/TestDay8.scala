package aoctest.aoc2023test.day8

import aoc.aoc2023.day8.Day8Puzzle
import aoctest.PuzzleTest
import org.junit.Test

class TestDay8 extends PuzzleTest(Day8Puzzle) {

  val testInputPart1_1Path = "src/test/scala/aoc2023test/day8/testInput_Day8Part1_1.txt"
  val testInputPart1_2Path = "src/test/scala/aoc2023test/day8/testInput_Day8Part1_2.txt"
  val testInputPart2Path = "src/test/scala/aoc2023test/day8/testInput_Day8Part2.txt"


  @Test
  def testDay8Part1_1(): Unit = {
    testPart1("2", inputPath = Some(testInputPathPart1Sub1))
  }

  @Test
  def testDay8Part1_2(): Unit = {
    testPart1("6", inputPath = Some(testInputPathPart1Sub2))
  }

  @Test
  def testDay8Part1real(): Unit = {
    testPart1Real("17263")
  }

  @Test
  def testDay8Part2(): Unit = {
    testPart2("6")
  }

  @Test
  def testDay8Part2real(): Unit = {
    testPart2Real("14631604759649")
  }
}
