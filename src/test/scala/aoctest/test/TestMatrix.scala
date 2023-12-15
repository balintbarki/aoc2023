package aoctest.test

import aoc.utils.{Matrix, NumericMatrix}
import org.junit.{Assert, Test}

class TestMatrix {
  private val testData1 = List()
  private val testData2 = List(List())
  private val testData2by2 = List(List(1, 2), List(3, 4))
  private val testData2by2Double = List(List(1.4, 2.3), List(3.1, 4.9))
  private val testData4by2 = List(List(1, 2, 3, 4), List(5, 6, 7, 8))
  private val testData3by3 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
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
    Matrix(testDataInvalid).transpose
  }

  @Test
  def testInequality(): Unit = {
    val testData2by2Other = Matrix(List(List(1, 2), List(3, 5)))
    val expected = NumericMatrix(List(List(0, 0), List(0, 1)))
    Assert.assertEquals(expected, Matrix(testData2by2) ~= testData2by2Other)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testInequalityInvalid1(): Unit = {
    val testData2by2Other = Matrix(List(List(1, 2), List(3, 5, 6)))
    val expected = Matrix(List(List(0, 0), List(0, 1)))
    Assert.assertEquals(expected, Matrix(testData2by2) ~= testData2by2Other)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testInequalityInvalid2(): Unit = {
    val testData2by2Other = Matrix(List(List(1, 2), List(3, 5), List(6, 7)))
    val expected = Matrix(List(List(0, 0), List(0, 1)))
    Assert.assertEquals(expected, Matrix(testData2by2) ~= testData2by2Other)
  }

  @Test
  def testNumericSumAll1(): Unit = {
    Assert.assertEquals(10, NumericMatrix(testData2by2).sumAll)
  }

  @Test
  def testNumericSumAll2(): Unit = {
    Assert.assertEquals(11.7, NumericMatrix(testData2by2Double).sumAll, 0.00001)
  }

  @Test
  def testMirrorAtRow0(): Unit = {
    Assert.assertEquals(Matrix(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))), Matrix(testData3by3).mirrorAtRow(0))
  }

  @Test
  def testMirrorAtRow1(): Unit = {
    Assert.assertEquals(Matrix(List(List(4, 5, 6), List(1, 2, 3), List(7, 8, 9))), Matrix(testData3by3).mirrorAtRow(1))
  }

  @Test
  def testMirrorAtRow2(): Unit = {
    Assert.assertEquals(Matrix(List(List(1, 2, 3), List(7, 8, 9), List(4, 5, 6))), Matrix(testData3by3).mirrorAtRow(2))
  }

  @Test
  def testMirrorAtRow3(): Unit = {
    Assert.assertEquals(Matrix(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))), Matrix(testData3by3).mirrorAtRow(3))
  }
}
