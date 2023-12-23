package aoc.utils.geometry

case class Point(x: Long, y: Long, z: Long = 0) {

  def distance(other: Point): Double = math
    .sqrt(math.pow(this.x - other.x, 2) + math.pow(this.y - other.y, 2) + math.pow(this.z - other.z, 2))
}
