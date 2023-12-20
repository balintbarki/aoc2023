package aoc.utils

// "start" is inclusive, "end" is exclusive
case class Range(start: Long, end: Long) extends Ordered[Range] {
  override def compare(that: Range): Int = start.compare(that.start)

  def length: Long = end - start

  // Splits the range at the specified value. The value is in the second range
  def splitAt(value: Long): (Option[Range], Option[Range]) = if (value <= start) {
    (None, Some(this))
  } else if (value < end) {
    (Some(Range(start, value)), Some(Range(value, end)))
  } else {
    (Some(this), None)
  }

}

object Range {
  def apply(start: Long, end: Long) = new Range(start, end)

  def apply(start: Long) = new Range(start, start + 1)
}
