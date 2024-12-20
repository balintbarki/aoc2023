package aoc.utils

trait Matrix[T] {

  lazy val xSize: Int = elements.headOption.map(_.length).getOrElse(0)
  lazy val ySize: Int = elements.length

  override final def equals(obj: Any): Boolean = obj match {
    case matrix: Matrix[_] => elements == matrix.elements
    case _                 => false
  }

  override final def hashCode(): Int = elements.map(_.hashCode()).hashCode()

  def elements: scala.collection.Seq[scala.collection.Seq[T]]

  def flattenedElements: scala.collection.Seq[T] = elements.flatten

  final def get(x: Int, y: Int): T = elements(y)(x)

  def count(p: T => Boolean): Int = elements.map(row => row.count(p)).sum

  def find(p: T => Boolean): Option[T] = {
    elements.flatten.find(p)
  }


}
