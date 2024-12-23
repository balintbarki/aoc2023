package aoctest.aoc2024test.day16

import aoc.aoc2024.day16.Day16Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

class TestDay16 extends PuzzleTest(Day16Puzzle) {

  //
  // Tests must be ran one by one, because apparently there is a dependency between them and
  // some of them fails if ran together
  //
  @Test
  def testDay16Part1(): Unit = {
    testPart1("7036")
  }

  @Test
  def testDay16Part1real(): Unit = {
    testPart1Real("83432")
  }

  @Test
  def testDay16Part2(): Unit = {
    testPart2("45")
  }

  @Test
  def testDay16Part2real(): Unit = {
    testPart2Real("467")
  }
}
