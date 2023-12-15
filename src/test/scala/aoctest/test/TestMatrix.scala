package aoctest.test

import aoc.utils.Matrix
import org.junit.{Assert, Test}

class TestMatrix {
  private val testData1 = List()
  private val testData2 = List(List())
  private val testData2by2 = List(List(1, 2), List(3, 4))
  private val testData4by2 = List(List(1, 2, 3, 4), List(5, 6, 7, 8))
  private val testDataInvalid = List(List(1), List(2, 3), List(4, 5, 6))

  @Test
  def testTranspose1(): Unit = {
    Assert.assertEquals(Matrix(List()), Matrix(testData1).transpose)
  }

  @Test
  def testTranspose2(): Unit = {
    Assert.assertEquals(Matrix(List(List())), Matrix(testData2).transpose)
  }

  @Test
  def testTranspose2by2(): Unit = {
    Assert.assertEquals(Matrix(List(List(1, 3), List(2, 4))), Matrix(testData2by2).transpose)
  }

  @Test
  def testTranspose4by2(): Unit = {
    Assert.assertEquals(Matrix(List(List(1, 5), List(2, 6), List(3, 7), List(4, 8))), Matrix(testData4by2).transpose)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testApplyInvalid(): Unit = {
    Assert.assertEquals(List(), Matrix(testDataInvalid).transpose)
  }
}
