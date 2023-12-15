package aoc.utils

object Combinatory {

  def combinations(n: Int, k: Int, min: Int = 0): List[List[Int]] = {

    val memoC = collection.mutable.Map.empty[(Int, Int, Int), List[List[Int]]]

    def combinationsWithHead(x: Int): List[List[Int]] = combinations(n - x, k - 1, x) map (x :: _)

    k match {
      case 0 => Nil
      case 1 => List(List(n))
      case _ =>
        memoC getOrElseUpdate((n, k, min),
          (min to n / 2).toList flatMap combinationsWithHead)
    }
  }

  def permutations(xs: List[Int]): List[List[Int]] = xs match {
    case Nil          => List(List())
    case head :: tail =>
      val len = xs.length
      val tps = (0 until len).map(xs.splitAt).toList.filter(tp => !tp._1.contains(tp._2.head))
      tps.flatMap(tp => permutations(tp._1 ::: tp._2.tail).map(tp._2.head :: _))
  }
}

object CombinatoryTest extends App {
  Combinatory.combinations(10, 4, 0).foreach(println(_))
}
