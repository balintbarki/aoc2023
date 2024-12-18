package aoctest.aoc2024test.day13

import aoc.aoc2024.day13.Day13Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

@Ignore
class TestDay13 extends PuzzleTest(Day13Puzzle) {

  @Test
  def testDay13Part1(): Unit = {
    testPart1("480")
  }

  @Test
  def testDay13Part1real(): Unit = {
    testPart1Real("31623")
  }

  @Test
  def testDay13Part2(): Unit = {
    testPart2("875318608908")
  }

  @Test
  def testDay13Part2real(): Unit = {
    testPart2Real("93209116744825")
  }
}
