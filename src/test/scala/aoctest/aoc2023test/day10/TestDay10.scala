package aoctest.aoc2023test.day10

import aoc.aoc2023.day10._
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay10 extends PuzzleTest(Day10Puzzle) {

  @Test
  def testDay10Part1_1(): Unit = {
    testPart1("4", inputPath = Some(testInputPathPart1Sub1))
  }

  @Test
  def testDay10Part1_2(): Unit = {
    testPart1("8", inputPath = Some(testInputPathPart1Sub2))
  }

  @Test
  def testDay10Part1real(): Unit = {
    testPart1Real("6875")
  }

  @Test
  def testDay10Part2_1(): Unit = {
    testPart2("4", inputPath = Some(testInputPathPart2Sub1))
  }

  @Test
  def testDay10Part2_2(): Unit = {
    testPart2("8", inputPath = Some(testInputPathPart2Sub2))
  }

  @Test
  def testDay10Part2_3(): Unit = {
    testPart2("10", inputPath = Some(testInputPathPart2Sub3))
  }

  @Test
  def testDay10Part2real(): Unit = {
    testPart2Real("471")
  }
}
