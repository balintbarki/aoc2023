package aoc2023test.day4

import aoc2023.day4.{Day4App, Day4_Part1, Day4_Part2}
import org.junit.{Assert, Test}

class TestDay4 {

  val testInputPath = "src/test/scala/aoc2023test/day4/testInput_Day4Part1.txt"

  @Test
  def testDay4Part1_real(): Unit = {
    Assert.assertEquals(19135, Day4_Part1.calculate(Day4App.inputPath))
  }

  @Test
  def testDay4Part2_real(): Unit = {
    Assert.assertEquals(5704953, Day4_Part2.calculate(Day4App.inputPath))
  }

  @Test
  def testDay4Part1(): Unit = {
    Assert.assertEquals(13, Day4_Part1.calculate(testInputPath))
  }

  @Test
  def testDay4Part2(): Unit = {
    Assert.assertEquals(30, Day4_Part2.calculate(testInputPath))
  }
}
