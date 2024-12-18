package aoc.aoc2024.day13

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.ImplicitUtils.AddMultispanToSeq
import aoc.utils.math.DiophantineEquation

import scala.collection.mutable

case object Day13Puzzle extends DailyPuzzle2024(13, "Claw Contraption") {

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, 0, 100)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, 10000000000000L, Long.MaxValue)

  private def calculate(lines: Seq[String], unitConversionToAdd: Long, maxButtonPushCnt: Long) =
    parseInput(lines, unitConversionToAdd).flatMap(_.findCheapestWin).sum


  private def parseInput(lines: Seq[String], unitConversionToAdd: Long) = {
    val buttonRegex = """Button .: X\+(\d+), Y\+(\d+)""".r
    val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r
    lines.multiSpanWithoutDelimiter(_.isEmpty).map { config =>
      val (ax, ay) = config.head match {
        case buttonRegex(x, y) => (x.toInt, y.toInt)
      }
      val (bx, by) = config(1) match {
        case buttonRegex(x, y) => (x.toInt, y.toInt)
      }
      val (prizeX, prizeY) = config(2) match {
        case prizeRegex(x, y) => (x.toInt, y.toInt)
      }

      MachineConfig(ax, ay, bx, by, prizeX + unitConversionToAdd, prizeY + unitConversionToAdd)
    }
  }

  private case class MachineConfig(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long) {

    private val tokensForA = 3L
    private val tokensForB = 1L

    def findCheapestWin: Option[Long] = {

      val B_denominator = ay * bx - ax * by
      if (B_denominator != 0) {
        val B_nominator = ay * px - ax * py

        if (B_nominator % B_denominator == 0) {
          val B = (ay * px - ax * py) / (bx * ay - by * ax)
          val A_nominator = px - bx * B

          if (A_nominator % ax == 0) {
            val A = A_nominator / ax
            Some(A * tokensForA + B * tokensForB)
          }
          else
            None
        }
        else
          None
      }
      else
        None
    }
  }
}
