package aoc.utils.math

import scala.annotation.tailrec


object Math {

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def gcdExt(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = if (b == 0) {
    (a.abs, 1, 0)
  } else {
    val (g, x1, y1) = gcdExt(b, a % b)
    val x = y1
    val y = x1 - y1 * (a / b)
    (g, x, y)
  }

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)
}
