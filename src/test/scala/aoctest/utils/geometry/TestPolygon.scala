package aoctest.utils.geometry

import aoc.utils.geometry.Polygon
import aoc.utils.geometry.Point
import org.junit.{Assert, Test}

class TestPolygon {

  @Test
  def testArea(): Unit = {
    val testPolygon = Polygon(List(
      Point(2, 7),
      Point(10, 1),
      Point(8, 6),
      Point(11, 7),
      Point(7, 10),
    ))
    Assert.assertEquals(32, testPolygon.area)
  }

  @Test
  def testPerimeter(): Unit = {
    val testPolygon = Polygon(List(
      Point(6, 0),
      Point(6, -5),
      Point(4, -5),
      Point(4, -7),
      Point(6, -7),
      Point(6, -9),
      Point(1, -9),
      Point(1, -7),
      Point(0, -7),
      Point(0, -5),
      Point(2, -5),
      Point(2, -2),
      Point(0, -2),
      Point(0, 0),
    ))

    Assert.assertEquals(42, testPolygon.area)
    Assert.assertEquals(38, testPolygon.perimeter.toLong)
  }
}
