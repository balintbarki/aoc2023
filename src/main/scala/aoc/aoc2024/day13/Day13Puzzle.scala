package aoc.aoc2024.day13

import aoc.aoc2024.DailyPuzzle2024
import aoc.utils.ImplicitUtils.AddMultispanToSeq

import scala.collection.immutable.NumericRange

case object Day13Puzzle extends DailyPuzzle2024(13, "Claw Contraption") {

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, 0, 100)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, 10000000000000L, Long.MaxValue)

  private def calculate(lines: Seq[String], unitConversionToAdd: Long, maxButtonPushCnt: Long) =
    parseInput(lines, unitConversionToAdd).flatMap(_.findCheapestWin(maxButtonPushCnt)).sum


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

  private case class MachineConfig(ax: Long, ay: Long, bx: Long, by: Long, prizeX: Long, prizeY: Long) {

    private val tokensForA = 3L
    private val tokensForB = 1L

    def findCheapestWin(maxButtonPush: Long): Option[Long] = {
      val maxA: Long = Seq(prizeX / ax, prizeY / ay).max.min(maxButtonPush)

      var fewestTokensOpt: Option[Long] = None
      var aPushCnt = 0
      while (aPushCnt < maxA) {

        val remainingX = prizeX - aPushCnt * ax
        val remainingY = prizeY - aPushCnt * ay

        val expectedBCnt = remainingX / bx
        if ((expectedBCnt == remainingY / by) && (remainingX % bx == 0) && (remainingY % by == 0)) {
          val tokens = aPushCnt * tokensForA + expectedBCnt * tokensForB
          fewestTokensOpt match {
            case None               => fewestTokensOpt = Some(tokens)
            case Some(fewestTokens) => if (tokens < fewestTokens) fewestTokensOpt = Some(tokens)
          }
        }

        aPushCnt += 1
      }

      fewestTokensOpt
    }
  }
}
