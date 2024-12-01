package aoctest.aoc2024test.day1

import aoc.aoc2024.day1.Day1Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay1 extends PuzzleTest(Day1Puzzle) {

  @Test
  def testDay1Part1(): Unit = {
    testPart1("11")
  }

  @Test
  def testDay1Part1real(): Unit = {
    testPart1Real("1222801")
  }

  @Test
  def testDay1Part2(): Unit = {
    testPart2("31")
  }

  @Test
  def testDay1Part2real(): Unit = {
    testPart2Real("22545250")
  }
}
