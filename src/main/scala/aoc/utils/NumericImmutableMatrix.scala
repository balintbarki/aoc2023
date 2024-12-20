package aoc.utils

class NumericImmutableMatrix[T: Numeric](elements: List[List[T]]) extends ImmutableMatrix[T](elements) {
  def sumAll(implicit num: Numeric[T]): T = {
    elements.foldLeft(num.zero)(
      (rowAcc, row) => num.plus(rowAcc, row.foldLeft(num.zero)((colAcc, elem) => num.plus(colAcc, elem))))
  }
}

object NumericImmutableMatrix {
  def apply[T: Numeric](elements: List[List[T]]): NumericImmutableMatrix[T] = new NumericImmutableMatrix(elements)
}
