package aoc.aoc2023.day5

final case class RangeShiftDefinition(
  range: Range,
  shift: Long) extends Ordered[RangeShiftDefinition] {
  override def compare(that: RangeShiftDefinition): Int = range.compare(that.range)

  def shiftRange: Range = Range(range.start + shift, range.end + shift)
}
