package aoc.utils

object Combinatory {

  val memoC = collection.mutable.Map.empty[(Int, Int, Int), List[List[Int]]]

  def combinations(n: Int, k: Int, min: Int = 0): List[List[Int]] = {
    def combinationsWithHead(x: Int) = combinations(n - x, k - 1, x) map (x :: _)

    k match {
      case 0 => Nil
      case 1 => List(List(n))
      case _ =>
        memoC getOrElseUpdate((n, k, min),
          (min to n / 2).toList flatMap combinationsWithHead)
    }
  }
}
