package aoctest.aoc2024test.day15

import aoc.aoc2024.day15.Day15Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay15 extends PuzzleTest(Day15Puzzle) {

  @Test
  def testDay15Part1(): Unit = {
    testPart1("10092")
  }

  @Test
  def testDay15Part1real(): Unit = {
    testPart1Real("0")
  }

  @Test
  def testDay15Part2(): Unit = {
    testPart2("0")
  }

  @Test
  def testDay15Part2real(): Unit = {
    testPart2Real("0")
  }
}
