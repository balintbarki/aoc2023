package aoc.aoc2024.day7

import aoc.aoc2024.DailyPuzzle2024

case object Day7Puzzle extends DailyPuzzle2024(7, "Bridge Repair") {

  override def calculatePart1(lines: Seq[String]): Long = calculate(lines, allowConcatenate = false)

  override def calculatePart2(lines: Seq[String]): Long = calculate(lines, allowConcatenate = true)

  private def calculate(lines: Seq[String], allowConcatenate: Boolean): Long = {
    val inputs = getInputs(lines)
    inputs.filter { case (result, operands) =>
      isEquationPossible(result, operands, allowConcatenate)
    }
      .map { case (result, _) => result }
      .sum
  }

  private def isEquationPossible(result: Long, operands: Seq[Long], allowConcatenate: Boolean): Boolean = {
    if (1 == operands.size) {
      result == operands.head
    } else {
      val lastOperand = operands.last
      val remainingOperands = operands.dropRight(1)

      def checkSumIsPossible = if (result > lastOperand) {
        isEquationPossible(result - lastOperand, remainingOperands, allowConcatenate)
      }
      else
        false

      def checkProductIsPossible = if (result % lastOperand == 0) {
        isEquationPossible(result / lastOperand, remainingOperands, allowConcatenate)
      }
      else
        false

      def checkConcatenateIsPossible = if (allowConcatenate && result.toString.endsWith(lastOperand.toString)) {
        val resultIfConcatenate = result.toString.dropRight(lastOperand.toString.length).toLong
        isEquationPossible(resultIfConcatenate, remainingOperands, allowConcatenate)
      }
      else
        false

      checkSumIsPossible || checkProductIsPossible || checkConcatenateIsPossible
    }
  }

  private def getInputs(lines: Seq[String]) = {
    lines.map(line => line.split(":").toSeq match {
      case Seq(result, operands) => (result.toLong, operands.trim.split(" ").map(_.toLong).toSeq)
    })
  }
}
