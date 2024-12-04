package aoc.utils

// "start" is inclusive, "end" is exclusive
case class LongRange(start: Long, end: Long) extends Ordered[LongRange] {
  override def compare(that: LongRange): Int = start.compare(that.start)

  def length: Long = end - start

  // Splits the range at the specified value. The value is in the second range
  def splitAt(value: Long): (Option[LongRange], Option[LongRange]) = if (value <= start) {
    (None, Some(this))
  } else if (value < end) {
    (Some(LongRange(start, value)), Some(LongRange(value, end)))
  } else {
    (Some(this), None)
  }

}

object LongRange {
  def apply(start: Long, end: Long) = new LongRange(start, end)

  def apply(start: Long) = new LongRange(start, start + 1)
}
