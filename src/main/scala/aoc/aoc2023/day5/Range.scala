package aoc.aoc2023.day5

// "start" is inclusive, "end" is exclusive
case class Range(start: Long, end: Long) extends Ordered[Range] {
  override def compare(that: Range): Int = start.compare(that.start)
}

object Range {
  def apply(start: Long, end: Long) = new Range(start, end)

  def apply(start: Long) = new Range(start, start + 1)
}
