package aoc.utils

import scala.collection.mutable

class NumericMatrix[T: Numeric](elements: mutable.Seq[mutable.Seq[T]]) extends Matrix[T](elements) {
  def sumAll(implicit num: Numeric[T]): T = {
    elements.foldLeft(num.zero)(
      (rowAcc, row) => num.plus(rowAcc, row.foldLeft(num.zero)((colAcc, elem) => num.plus(colAcc, elem))))
  }
}

object NumericMatrix {
  def apply[T: Numeric](elements: Seq[Seq[T]]): NumericMatrix[T] = new NumericMatrix(
    mutable.Seq.from(elements.map(mutable.Seq.from(_))))
}
