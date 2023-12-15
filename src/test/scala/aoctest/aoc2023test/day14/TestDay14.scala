package aoctest.aoc2023test.day14

import aoc.aoc2023.day14.Day14Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay14 extends PuzzleTest(Day14Puzzle) {

  @Test
  def testDay14Part1(): Unit = {
    testPart1("136")
  }

  @Test
  def testDay14Part1real(): Unit = {
    testPart1Real("110407")
  }

  @Test
  def testDay14Part2(): Unit = {
    testPart2("64")
  }

  @Test
  def testDay14Part2real(): Unit = {
    testPart2Real("87273")
  }
}
