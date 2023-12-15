package aoc.utils

case class Matrix[T](elements: Seq[Seq[T]]) {

  if (elements.nonEmpty && elements.head.nonEmpty)
    require(elements.forall(row => row.length == elements.head.length),
      s"Matrix rows shall have equal number of elements. Row lengths: ${elements.map(_.length).mkString(", ")}")

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
