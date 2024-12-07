package aoc.utils

class NumericMatrix[T: Numeric](elements: List[List[T]]) extends Matrix[T](elements) {
  def sumAll(implicit num: Numeric[T]): T = {
    elements.foldLeft(num.zero)(
      (rowAcc, row) => num.plus(rowAcc, row.foldLeft(num.zero)((colAcc, elem) => num.plus(colAcc, elem))))
  }
}

object NumericMatrix {
  def apply[T: Numeric](elements: List[List[T]]): NumericMatrix[T] = new NumericMatrix(elements)
}
