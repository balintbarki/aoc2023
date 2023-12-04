package aoc2023test.day2

import aoc2023.day2._
import org.junit.{Assert, Test}

class TestDay2 {

  val testInputPath = "src/test/scala/aoc2023test/day2/testInput_Day2Part1.txt"

  @Test
  def testDay2Part1(): Unit = {
    Assert.assertEquals(8, Day2_Part1.calculate(testInputPath))
  }

  @Test
  def testDay2Part2(): Unit = {
    Assert.assertEquals(2286, Day2_Part2.calculate(testInputPath))
  }

  @Test
  def testDay2Part2_real(): Unit = {
    Assert.assertEquals(66909, Day2_Part2.calculate(Day2App.inputPath))
  }
}
