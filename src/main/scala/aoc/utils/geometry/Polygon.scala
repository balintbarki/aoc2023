package aoc.utils.geometry

case class Polygon(points: List[Point]) {

  /**
   * Area using the Shoelace formula as described here
   * https://www.101computing.net/the-shoelace-algorithm/
   *
   * @return Area of the polygon
   */
  def area: Long = {
    require(points.forall(_.z == 0), "Area calculation works only for 2D polygons (each points have z = 0)")
    (points.indices.dropRight(1).map(i => points(i).x * points(i + 1).y).sum + points.last.x * points.head.y -
      points.indices.dropRight(1).map(i => points(i + 1).x * points(i).y).sum - points.head.x * points.last.y).abs / 2
  }

  def perimeter: Double = {
    (points :+ points.head).sliding(2).map { case Seq(first, second) => first.distance(second) }.sum
  }

}
