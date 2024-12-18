package aoc.utils.math

// https://cp-algorithms.com/algebra/linear-diophantine-equation.html
// A Linear Diophantine Equation (in two variables) is an equation of the general form:
//
// ax + by = c
//
//where a, b, c are given integers, and x, y are unknown integers.
//
class DiophantineEquation(a: BigInt, b: BigInt, c: BigInt) {

  require(a >= 0, "a must be non-negative")
  require(b >= 0, "b must be non-negative")

  lazy val (g, xg, yg) = Math.gcdExt(a, b)
  lazy val ag: BigInt = a / g
  lazy val bg: BigInt = b / g


  def hasSolution: Boolean = c % g == 0

  def solveForX(x: BigInt): BigInt = (c - a * x) / b

  def solveForY(y: BigInt): BigInt = (c - b * y) / a

  def allSolutionsXValues(minX: BigInt, maxX: BigInt, minY: BigInt, maxY: BigInt): Option[(BigInt, BigInt, BigInt)] = {
    val sign_a = if (a > 0) 1 else -1
    val sign_b = if (b > 0) 1 else -1
    var lx1: BigInt = BigInt(0)
    var rx1: BigInt = BigInt(0)
    var lx2: BigInt = BigInt(0)
    var rx2: BigInt = BigInt(0)

    anySolution.flatMap { case (x0, y0) =>
      // This is coupled together as var (x, y) can't be reassigned for some reason
      var xy = shiftSolution(x0, y0, (minX - x0) / bg)
      if (xy._1 < minX)
        xy = shiftSolution(xy._1, xy._2, sign_b)
      if (xy._1 > maxX)
        None
      else {
        lx1 = xy._1
        Some(xy)
      }
    }.map { case (x, y) =>
      var xy = shiftSolution(x, y, (maxX - x) / bg)
      if (xy._1 > maxX)
        xy = shiftSolution(xy._1, xy._2, -sign_b)
      rx1 = xy._1
      xy
    }.flatMap { case (x, y) =>
      var xy = shiftSolution(x, y, -(minY - y) / ag)
      if (xy._2 < minY)
        xy = shiftSolution(xy._1, xy._2, -sign_a)
      if (xy._2 > maxY)
        None
      else {
        lx2 = xy._1
        Some(xy)
      }
    }.map { case (x, y) =>
      var xy = shiftSolution(x, y, -(maxY - y) / ag)
      if (xy._2 > maxY)
        xy = shiftSolution(xy._1, xy._2, sign_a)
      rx2 = xy._1
      xy
    } match {
      case Some(_) =>
        if (lx2 > rx2) {
          val tmp = rx2
          rx2 = lx2
          lx2 = tmp
        }
        val lx = lx1.max(lx2)
        val rx = rx1.min(rx2)
        val step = b / g

        Some(lx, rx, step)

      case _ => None
    }
  }

  // Find all solutions that are in the form of:
  // x = x0 + k * (b / g)
  // y = y0 + k * (a / g)
  // and also limiting their value
  def allSolutions(minX: BigInt, maxX: BigInt, minY: BigInt, maxY: BigInt): Seq[(BigInt, BigInt)] = {
    allSolutionsXValues(minX, maxX, minY, maxY) match {
      case Some((minX, maxX, step)) =>
        (minX to(maxX, step)).map { x => (x, solveForX(x)) }

      case _ => Seq.empty
    }
  }

  def anySolution: Option[(BigInt, BigInt)] = {
    if (c % g != 0)
      None
    else {
      val x0 = xg * c / g * {
        if (a < 0) -1 else 1
      }
      val y0 = yg * c / g * {
        if (b < 0) -1 else 1
      }
      Some(x0, y0)
    }
  }

  override def toString = s"${a}x + ${b}y = $c"

  private def shiftSolution(x0: BigInt, y0: BigInt, cnt: BigInt): (BigInt, BigInt) = {
    (x0 + cnt * bg, y0 - cnt * ag)
  }
}
