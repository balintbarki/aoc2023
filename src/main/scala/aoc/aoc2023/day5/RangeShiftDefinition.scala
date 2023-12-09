package aoc.aoc2023.day5

import scala.collection.immutable

final case class RangeShiftDefinition(
  range: Range,
  shift: Long) extends Ordered[RangeShiftDefinition] {
  override def compare(that: RangeShiftDefinition): Int = range.compare(that.range)

  def doShift: Range = Range(range.start + shift, range.end + shift)
}
