package aoctest.aoc2024test.day5

import aoc.aoc2024.day5.Day5Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay5 extends PuzzleTest(Day5Puzzle) {

  @Test
  def testDay5Part1(): Unit = {
    testPart1("143")
  }

  @Test
  def testDay5Part1real(): Unit = {
    testPart1Real("7024")
  }

  @Test
  def testDay5Part2(): Unit = {
    testPart2("123")
  }

  @Test
  def testDay5Part2real(): Unit = {
    testPart2Real("4151")
  }
}
