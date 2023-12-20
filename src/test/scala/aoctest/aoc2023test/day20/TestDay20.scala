package aoctest.aoc2023test.day20

import aoc.aoc2023.day20.Day20Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}


class TestDay20 extends PuzzleTest(Day20Puzzle) {

  @Test
  def testDay20Part1_1(): Unit = {
    testPart1("32000000", inputPath = Some(testInputPathPart1Sub1))
  }

  @Test
  def testDay20Part1_2(): Unit = {
    testPart1("11687500", inputPath = Some(testInputPathPart1Sub2))
  }

  @Test
  def testDay20Part1real(): Unit = {
    testPart1Real("834323022")
  }

  @Ignore
  @Test
  def testDay20Part2real(): Unit = {
    testPart2Real("0")
  }
}
