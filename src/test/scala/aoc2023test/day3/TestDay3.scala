package aoc2023test.day3

import aoc2023.day3._
import org.junit.{Assert, Test}

class TestDay3 {

  val testInputPath = "src/test/scala/aoc2023test/day3/testInput_Day3Part1.txt"

  @Test
  def testDay3Part1_real(): Unit = {
    Assert.assertEquals(554003, Day3_Part1.calculate(Day3App.inputPath))
  }

  @Test
  def testDay3Part1(): Unit = {
    Assert.assertEquals(4361, Day3_Part1.calculate(testInputPath))
  }

  @Test
  def testDay3Part2(): Unit = {
    Assert.assertEquals(467835, Day3_Part2.calculate(testInputPath))
  }
}
