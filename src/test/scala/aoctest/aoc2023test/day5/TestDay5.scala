package aoctest.aoc2023test.day5

import aoc.aoc2023.day5
import aoc.aoc2023.day5._
import org.junit.{Assert, Test}

class TestDay5 {
  val testInputPath = "src/test/scala/aoc2023test/day5/testInput_Day5Part1.txt"

  @Test
  def testIntersectRangeWithRange_no_overlap1(): Unit = {
    Assert
      .assertEquals(Seq(day5.Range(2, 41)),
        Day5_Part2
          .intersectRangeWithRangeShiftDefinition(day5.Range(2, 41), RangeShiftDefinition(day5.Range(41, 642), 0)))
  }

  @Test
  def testIntersectRangeWithRange_no_overlap2(): Unit = {
    Assert.assertEquals(Seq(day5.Range(41, 642)),
      Day5_Part2
        .intersectRangeWithRangeShiftDefinition(day5.Range(41, 642), RangeShiftDefinition(day5.Range(2, 41), 0)))
  }

  @Test
  def testIntersectRangeWithRange_overlap_first_starts_first(): Unit = {
    Assert
      .assertEquals(Seq(day5.Range(10, 20), day5.Range(20, 30)),
        Day5_Part2
          .intersectRangeWithRangeShiftDefinition(day5.Range(10, 30), RangeShiftDefinition(day5.Range(20, 40), 0)))
  }

  @Test
  def testIntersectRangeWithRange_overlap_second_starts_first(): Unit = {
    Assert
      .assertEquals(Seq(day5.Range(20, 30), day5.Range(30, 40)),
        Day5_Part2
          .intersectRangeWithRangeShiftDefinition(day5.Range(20, 40), RangeShiftDefinition(day5.Range(10, 30), 0)))
  }

  @Test
  def testIntersectRangeWithPropertyMap_1(): Unit = {
    val range = day5.Range(20, 40)
    val propertyMap = new PropertyMap(Seq(
      RangeShiftDefinition(day5.Range(50, 60), 0)
    )
    )
    Assert.assertEquals(Seq(day5.Range(20, 40)), Day5_Part2.intersectRangeWithPropertyMap(range, propertyMap))
  }

  @Test
  def testIntersectRangeWithPropertyMap_2(): Unit = {
    val range = day5.Range(70, 80)
    val propertyMap = new PropertyMap(Seq(
      RangeShiftDefinition(day5.Range(50, 60), 0)
    )
    )
    Assert.assertEquals(Seq(day5.Range(70, 80)), Day5_Part2.intersectRangeWithPropertyMap(range, propertyMap))
  }

  @Test
  def testIntersectRangeWithPropertyMap_3(): Unit = {
    val range = day5.Range(20, 40)
    val propertyMap = new PropertyMap(Seq(
      RangeShiftDefinition(day5.Range(0, 10), 0),
      RangeShiftDefinition(day5.Range(15, 25), 0),
      RangeShiftDefinition(day5.Range(35, 45), 0),
      RangeShiftDefinition(day5.Range(55, 65), 0),
    )
    )
    Assert.assertEquals(Seq(day5.Range(20, 25), day5.Range(25, 35), day5.Range(35, 40)),
      Day5_Part2.intersectRangeWithPropertyMap(range, propertyMap))
  }

  @Test
  def testIntersectRangesWithPropertyMap(): Unit = {
    val ranges = Seq(day5.Range(55, 68), day5.Range(79, 93))
    val propertyMap = new PropertyMap(Seq(
      RangeShiftDefinition(day5.Range(90, 100), -48),
      RangeShiftDefinition(day5.Range(50, 60), 2),
    )
    )
    Assert.assertEquals(Seq(day5.Range(55, 60), day5.Range(60, 68), day5.Range(79, 90), day5.Range(90, 93)),
      Day5_Part2.intersectRangesWithPropertyMap(ranges, propertyMap))
  }

  @Test
  def testPropertyMapLookup(): Unit = {
    val propertyMap = new PropertyMap(
      Seq(RangeShiftDefinition(day5.Range(98, 100), -48), RangeShiftDefinition(day5.Range(50, 98), 2)))

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
  def testDay5Part1_real(): Unit = {
    Assert.assertEquals(107430936, Day5_Part1.calculate(Day5App.inputPath))
  }

  @Test
  def testDay5Part1(): Unit = {
    Assert.assertEquals(35, Day5_Part1.calculate(testInputPath))
  }

  @Test
  def testDay5Part2(): Unit = {
    Assert.assertEquals(46, Day5_Part2.calculate(testInputPath))
  }
}
