package aoc.utils

class ImmutableMatrix[T](val elements: List[List[T]]) extends Matrix[T] {

  if (elements.nonEmpty && elements.head.nonEmpty)
    require(elements.forall(row => row.length == elements.head.length),
      s"Matrix rows shall have equal number of elements. Row lengths: ${elements.map(_.length).mkString(", ")}")


  def rows: List[List[T]] = elements

  def columns: List[List[T]] = transpose.rows

  def updated(x: Int, y: Int, item: T): ImmutableMatrix[T] = ImmutableMatrix(
    elements.updated(y, elements(y).updated(x, item)))

  def map[B](f: T => B): ImmutableMatrix[B] = ImmutableMatrix(
    elements.indices.map(row => elements.head.indices.map(column => f(elements(row)(column))).toList).toList)

  def transpose: ImmutableMatrix[T] =
    if (elements.isEmpty || elements.forall(_.isEmpty))
      this
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      ImmutableMatrix(
        columns.map(column => rows.foldLeft(List[T]())((acc, row) => acc :+ elements(row)(column))).toList)
    }

  def rotateLeft: ImmutableMatrix[T] = {
    if ((xSize == 0) || (ySize == 0)) {
      this
    }
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      ImmutableMatrix(columns.reverse.map { column =>
        rows.map { row =>
          val element = elements(row)(column)
          element
        }
      })
    }
  }

  def rotateRight: ImmutableMatrix[T] = {
    if ((xSize == 0) || (ySize == 0)) {
      this
    }
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      ImmutableMatrix(columns.map { column =>
        rows.reverse.map { row =>
          val element = elements(row)(column)
          element
        }
      })
    }
  }

  /** Calculates the inequality matrix of two matrices
   *
   * The inequality matrix contains 1 where the two matrices differ and 0 where they are equal.
   * The two input matrices need to have the same sizes.
   */
  def ~=(other: ImmutableMatrix[T]): NumericImmutableMatrix[Int] = {
    require(
      (this.elements.length == other.elements.length) && (this.elements.head.length == other.elements.head.length),
      "Number of rows and columns shall match for inequality")
    NumericImmutableMatrix(elements.indices
      .map(row => elements.head.indices
        .map(column => if (this.elements(row)(column) == other.elements(row)(column)) 0 else 1).toList).toList)
  }

  def mirrorAtRow(row: Int): ImmutableMatrix[T] = {
    if ((row < 1) || (this.rows.length <= row))
      this
    else {
      val takeCnt = List(this.rows.length - row, row).min
      this.rows.splitAt(row) match {
        case (first, second) => ImmutableMatrix(
          first.take(row - takeCnt) ++
            second.take(takeCnt).reverse ++
            first.takeRight(takeCnt).reverse ++
            second.drop(takeCnt))
      }
    }
  }

  def mirrorAtColumn(column: Int): ImmutableMatrix[T] = transpose.mirrorAtRow(column).transpose

  def getCoordinateMap: Map[(Int, Int), T] = rows.indices
    .flatMap(rowIdx => columns.indices.map(colIdx => (colIdx, rowIdx) -> get(colIdx, rowIdx))).toMap

  def print(minWidth: Int = 1, separator: Char = ' '): Unit = {
    rows.foreach(
      row => println(row.map(item => String.format("%" + minWidth + "s", item.toString)).mkString(separator.toString)))
  }
}

object ImmutableMatrix {

  def apply[T](elements: List[List[T]]): ImmutableMatrix[T] = new ImmutableMatrix(elements)

  def apply[T](elements: Seq[Seq[T]]): ImmutableMatrix[T] = new ImmutableMatrix(elements.map(_.toList).toList)

  def unapply[T](matrix: ImmutableMatrix[T]): List[List[T]] = matrix.elements

  def fromStrings(lines: List[String]): ImmutableMatrix[Char] = ImmutableMatrix(lines.map(_.toList))

  def fromStrings(lines: Seq[String]): ImmutableMatrix[Char] = fromStrings(lines.toList)

  def fromStringsToInts(lines: Seq[String]): ImmutableMatrix[Int] = ImmutableMatrix(
    lines.map(line => line.map(c => c - '0')).toList)


}
