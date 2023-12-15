package aoc.utils

object Parsing {

  def stringToNumbers(s: String): Seq[Long] = {
    """(-?\d+)""".r.findAllIn(s).matchData.map(_.matched.toLong).toSeq
  }
}
