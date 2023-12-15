package aoc.utils

class Matrix[T](val elements: List[List[T]]) {

  if (elements.nonEmpty && elements.head.nonEmpty)
    require(elements.forall(row => row.length == elements.head.length),
      s"Matrix rows shall have equal number of elements. Row lengths: ${elements.map(_.length).mkString(", ")}")

  override def equals(obj: Any): Boolean = obj match {
    case matrix: Matrix[_] => elements == matrix.elements
    case _                 => false
  }

  def rows: List[List[T]] = elements

  def columns: List[List[T]] = this.transpose.rows

  def transpose: Matrix[T] = {
    if (elements.isEmpty || elements.forall(_.isEmpty))
      this
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      Matrix(columns.map(column => rows.foldLeft(List[T]())((acc, row) => acc :+ elements(row)(column))).toList)
    }
  }

  /** Calculates the inequality matrix of two matrices
   *
   * The inequality matrix contains 1 where the two matrices differ and 0 where they are equal.
   * The two input matrices need to have the same sizes.
   */
  def ~=(other: Matrix[T]): NumericMatrix[Int] = {
    require(
      (this.elements.length == other.elements.length) && (this.elements.head.length == other.elements.head.length),
      "Number of rows and columns shall match for inequality")
    NumericMatrix(elements.indices
      .map(row => elements.head.indices
        .map(column => if (this.elements(row)(column) == other.elements(row)(column)) 0 else 1).toList).toList)
  }

  def mirrorAtRow(row: Int): Matrix[T] = {
    if ((row < 1) || (this.rows.length <= row))
      this
    else {
      val takeCnt = List(this.rows.length - row, row).min
      this.rows.splitAt(row) match {
        case (first, second) => Matrix(
          first.take(row - takeCnt) ++
            second.take(takeCnt).reverse ++
            first.takeRight(takeCnt).reverse ++
            second.drop(takeCnt))
      }
    }
  }

  def mirrorAtColumn(column: Int): Matrix[T] = transpose.mirrorAtRow(column).transpose

}

class NumericMatrix[T: Numeric](elements: List[List[T]]) extends Matrix[T](elements) {
  def sumAll(implicit num: Numeric[T]): T = {
    elements.foldLeft(num.zero)(
      (rowAcc, row) => num.plus(rowAcc, row.foldLeft(num.zero)((colAcc, elem) => num.plus(colAcc, elem))))
  }
}

object Matrix {

  def apply[T](elements: List[List[T]]): Matrix[T] = new Matrix(elements)

  def unapply[T](matrix: Matrix[T]): List[List[T]] = matrix.elements

  def fromStrings(lines: List[String]): Matrix[Char] = Matrix(lines.map(_.toList))
}

object NumericMatrix {
  def apply[T: Numeric](elements: List[List[T]]): NumericMatrix[T] = new NumericMatrix(elements)
}
