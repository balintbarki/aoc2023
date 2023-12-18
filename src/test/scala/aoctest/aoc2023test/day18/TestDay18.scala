package aoctest.aoc2023test.day18

import aoc.aoc2023.day18.Day18Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay18 extends PuzzleTest(Day18Puzzle) {

  @Test
  def testDay18Part1(): Unit = {
    testPart1("62")
  }

  @Test
  def testDay18Part1real(): Unit = {
    testPart1Real("76387")
  }

  @Test
  def testDay18Part2(): Unit = {
    testPart2("952408144115")
  }

  @Test
  def testDay18Part2real(): Unit = {
    testPart2Real("250022188522074")
  }
}
