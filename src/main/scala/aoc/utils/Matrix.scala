package aoc.utils

import scala.collection.mutable

class Matrix[T](val elements: mutable.Seq[mutable.Seq[T]]) {

  if (elements.nonEmpty && elements.head.nonEmpty)
    require(elements.forall(row => row.length == elements.head.length),
      s"Matrix rows shall have equal number of elements. Row lengths: ${elements.map(_.length).mkString(", ")}")

  lazy val xSize: Int = elements.headOption.map(_.length).getOrElse(0)
  lazy val ySize: Int = elements.length

  override def equals(obj: Any): Boolean = obj match {
    case matrix: Matrix[_] => elements == matrix.elements
    case _                 => false
  }

  override def hashCode(): Int = elements.map(_.hashCode()).hashCode()

  def rows: Seq[Seq[T]] = elements.map(_.toSeq).toSeq

  def columns: Seq[Seq[T]] = transpose.rows

  def flattenedElements: mutable.Seq[T] = elements.flatten

  def get(x: Int, y: Int): Option[T] = if ((0 <= x) && (x < xSize) && (0 <= y) && (y < ySize))
    Some(elements(y)(x))
  else
    None

  def get(coordinates: (Int, Int)): Option[T] = get(coordinates._1, coordinates._2)

  def getOrThrow(x: Int, y: Int): T = get(x, y)
    .getOrElse(throw new IllegalArgumentException(s"Matrix element not found at position ($x, $y)"))

  def getOrThrow(coordinates: (Int, Int)): T = getOrThrow(coordinates._1, coordinates._2)

  def update(x: Int, y: Int, item: T): Unit = elements(y).update(x, item)

  def map[B](f: T => B): Matrix[B] = Matrix(
    elements.indices.map(row => elements.head.indices.map(column => f(elements(row)(column))).toList).toList)

  def count(p: T => Boolean): Int = elements.map(row => row.count(p)).sum

  def find(p: T => Boolean): Option[T] = {
    elements.flatten.find(p)
  }

  def findOrThrow(p: T => Boolean): T = find(p).getOrElse(throw new IllegalArgumentException(s"Element not found"))

  def allElements: Seq[T] = elements.flatten.toSeq

  def transpose: Matrix[T] =
    if (elements.isEmpty || elements.forall(_.isEmpty))
      this
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      Matrix(columns.map(column => rows.foldLeft(List[T]())((acc, row) => acc :+ elements(row)(column))).toList)
    }

  def rotateLeft: Matrix[T] = {
    if ((xSize == 0) || (ySize == 0)) {
      this
    }
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      Matrix(columns.reverse.map { column =>
        rows.map { row =>
          val element = elements(row)(column)
          element
        }
      })
    }
  }

  def rotateRight: Matrix[T] = {
    if ((xSize == 0) || (ySize == 0)) {
      this
    }
    else {
      val rows = elements.indices
      val columns = elements.head.indices
      Matrix(columns.map { column =>
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
  def ~=(other: Matrix[T]): NumericMatrix[Int] = {
    require(
      (this.elements.length == other.elements.length) && (this.elements.head.length == other.elements.head.length),
      "Number of rows and columns shall match for inequality")
    NumericMatrix(elements.indices
      .map(row => elements.head.indices
        .map(column => if (this.elements(row)(column) == other.elements(row)(column)) 0 else 1)))
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

  def getCoordinateMap: Map[(Int, Int), T] = rows.indices
    .flatMap(
      rowIdx => columns.indices.flatMap(colIdx => get(colIdx, rowIdx).map(element => (colIdx, rowIdx) -> element)))
    .toMap

  def print(minWidth: Int = 1, separator: String = " "): Unit = {
    rows.foreach(
      row => println(row.map(item => String.format("%" + minWidth + "s", item.toString)).mkString(separator)))
  }

  def mirrorHorizontally(): Matrix[T] = Matrix(elements.indices.map { rowIdx => elements(rowIdx).reverse })

  def mirrorVertically(): Matrix[T] = Matrix(elements.reverse)
}

object Matrix {

  def apply[T](elements: mutable.Seq[mutable.Seq[T]]): Matrix[T] = new Matrix(elements)

  def apply[T](elements: scala.collection.Seq[scala.collection.Seq[T]]): Matrix[T] = new Matrix(
    mutable.Seq.from(elements.map(mutable.Seq.from(_))))

  def fromStrings(lines: List[String]): Matrix[Char] = Matrix(lines.map(_.toList))

  def fromStrings(lines: Seq[String]): Matrix[Char] = fromStrings(lines.toList)

  def fromStringsToInts(lines: Seq[String]): Matrix[Int] = Matrix(lines.map(line => line.map(c => c - '0')).toList)
}
