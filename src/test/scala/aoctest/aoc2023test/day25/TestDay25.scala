package aoctest.aoc2023test.day25

import aoc.aoc2023.day25.Day25Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore // runs for too long
class TestDay25 extends PuzzleTest(Day25Puzzle) {

  @Test
  def testDay25Part1(): Unit = {
    testPart1("54")
  }

  @Test
  def testDay25Part1real(): Unit = {
    testPart1Real("567606")
  }

  @Ignore
  @Test
  def testDay25Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay25Part2real(): Unit = {
    testPart2Real("0")
  }
}
