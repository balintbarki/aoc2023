package aoc.utils

class Matrix[T] private(val elements: Seq[Seq[T]]) {
  def transpose: Matrix[T] = {
    if (elements.isEmpty || elements.forall(_.isEmpty))
      this
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      new Matrix(columns.map(column => rows.foldLeft(Seq[T]())((acc, row) => acc :+ elements(row)(column))))
    }
  }
}

object Matrix {

  def apply[T](elements: Seq[Seq[T]]): Matrix[T] = {
    if (elements.nonEmpty && elements.head.nonEmpty)
      require(elements.forall(row => row.length == elements.head.length))
    new Matrix[T](elements)
  }

}
