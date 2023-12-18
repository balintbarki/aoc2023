package aoctest.utils.geometry

import aoc.utils.geometry.Point
import org.junit.{Assert, Test}

class TestPoint {

  @Test
  def testDistance(): Unit = {
    Assert.assertEquals(1, Point(0, 0).distance(Point(1, 0)), 0.0001)
    Assert.assertEquals(2, Point(0, 0).distance(Point(0, 2)), 0.0001)
    Assert.assertEquals(5, Point(1, 2).distance(Point(4, 6)), 0.0001)
  }
}
