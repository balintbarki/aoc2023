package aoc.utils

import scala.collection.mutable

class MutableMatrix[T](val elements: mutable.Seq[mutable.Seq[T]]) extends Matrix[T] {

  def map[B](f: T => B): MutableMatrix[B] = {
    val newElements = elements.indices.map(
      row => elements.head.indices.map(
        column =>
          f(elements(row)(column))))

    MutableMatrix(newElements)
  }

  def update(x: Int, y: Int, newValue: T): Unit = elements(y).update(x, newValue)
}

object MutableMatrix {
  def apply[T](elements: mutable.Seq[mutable.Seq[T]]): MutableMatrix[T] = new MutableMatrix(
    elements)

  def apply[T](elements: Seq[Seq[T]]): MutableMatrix[T] = {
    MutableMatrix(mutable.Seq.from(elements.map(line => mutable.Seq.from(line))))
  }
}
