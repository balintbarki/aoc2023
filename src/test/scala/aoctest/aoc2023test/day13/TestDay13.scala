package aoctest.aoc2023test.day13

import aoc.aoc2023.day13.Day13Puzzle
import aoctest.PuzzleTest
import org.junit.{Ignore, Test}

import java.nio.file.Paths

class TestDay13 extends PuzzleTest(Day13Puzzle) {

  @Ignore
  @Test
  def testDay12Debug(): Unit = {
    testPart1("8", Some(Paths.get(s"src/test/scala/aoctest/aoc2023test/day13/debugInput.txt")))
  }

  @Test
  def testDay13Part1(): Unit = {
    testPart1("405")
  }

  @Test
  def testDay13Part1real(): Unit = {
    testPart1Real("30518")
  }

  @Ignore
  @Test
  def testDay13Part2(): Unit = {
    testPart2("0")
  }

  @Ignore
  @Test
  def testDay13Part2real(): Unit = {
    testPart2Real("0")
  }
}
