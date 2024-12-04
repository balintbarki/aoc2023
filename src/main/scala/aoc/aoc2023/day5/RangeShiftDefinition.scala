package aoc.aoc2023.day5

import aoc.utils

final case class RangeShiftDefinition(
  range: utils.LongRange,
  shift: Long) extends Ordered[RangeShiftDefinition] {
  override def compare(that: RangeShiftDefinition): Int = range.compare(that.range)

  def shiftRange: utils.LongRange = utils.LongRange(range.start + shift, range.end + shift)
}
