package aoctest.utils

import aoc.utils.ImplicitUtils.AddMultispanToList
import org.junit.{Assert, Test}

class TestImplicitUtils {

  val testData1: List[Int] = List(1, 2, 3, 0, 4, 5, 0, 6, 7)
  val testData2: List[Int] = List(0, 1, 2, 3, 0, 0, 4, 5, 0, 6, 7, 0, 0, 0)

  @Test
  def testMultiSpan1(): Unit = {
    Assert.assertEquals(List(List(1, 2, 3), List(0, 4, 5), List(0, 6, 7)), testData1.multiSpan(_ == 0))
  }

  @Test
  def testMultiSpan2(): Unit = {
    Assert.assertEquals(List(List(0, 1, 2, 3), List(0), List(0, 4, 5), List(0, 6, 7), List(0), List(0), List(0)),
      testData2.multiSpan(_ == 0))
  }

  @Test
  def testMultiSpanWithoutDelimiter1(): Unit = {
    Assert.assertEquals(List(List(1, 2, 3), List(4, 5), List(6, 7)), testData1.multiSpanWithoutDelimiter(_ == 0))
  }

  @Test
  def testMultiSpanWithoutDelimiter2(): Unit = {
    Assert.assertEquals(List(List(1, 2, 3), List(4, 5), List(6, 7)), testData2.multiSpanWithoutDelimiter(_ == 0))
  }
}
