package aoctest.aoc2023test.day5

import aoc.aoc2023.day5
import aoc.aoc2023.day5._
import aoc.utils
import aoctest.PuzzleTest
import org.junit.{Assert, Test}

class TestDay5 extends PuzzleTest(Day5Puzzle) {
  val testInputPath222 = "src/test/scala/aoc2023test/day5/testInput_Day5Part1.txt"

  @Test
  def testPropertyMapLookup(): Unit = {
    val propertyMap = new PropertyMap(
      Seq(RangeShiftDefinition(utils.Range(98, 100), -48), RangeShiftDefinition(utils.Range(50, 98), 2)))

    val testData = Seq(
      (0, 0),
      (1, 1),
      (48, 48),
      (49, 49),
      (50, 52),
      (51, 53),
      (96, 98),
      (97, 99),
      (98, 50),
      (99, 51),
    )

    testData.foreach { case (src, dst) => Assert.assertEquals(dst, propertyMap.shift(src)) }
  }

  @Test
  def testPropertyRangeProcess(): Unit = {

  }


  @Test
  def testDay5Part1(): Unit = {
    testPart1("35")
  }

  @Test
  def testDay5Part2(): Unit = {
    testPart2("46")
  }

  @Test
  def testDay5Part1_real(): Unit = {
    testPart1Real("107430936")
  }

  @Test
  def testDay5Part2_real(): Unit = {
    testPart2Real("23738616")
  }

}
