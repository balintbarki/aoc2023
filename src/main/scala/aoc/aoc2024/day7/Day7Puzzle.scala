package aoc.aoc2024.day7

import aoc.aoc2024.DailyPuzzle2024

case object Day7Puzzle extends DailyPuzzle2024(7, "Bridge Repair") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val inputs = getInputs(lines)
    inputs.filter { case (result, operands) =>
      isEquationPossible(result, operands)
    }
      .map { case (result, _) => result }
      .sum
  }

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def isEquationPossible(result: Long, operands: Seq[Long]): Boolean = {
    if (1 == operands.size) {
      result == operands.head
    } else {
      val lastOperand = operands.last
      val resultIfSum = result - lastOperand
      val resultIfProduct = result / lastOperand

      isEquationPossible(resultIfSum, operands.dropRight(1)) || {
        if (result % lastOperand == 0)
          isEquationPossible(resultIfProduct, operands.dropRight(1))
        else
          false
      }
    }
  }

  private def getInputs(lines: Seq[String]) = {
    lines.map(line => line.split(":").toSeq match {
      case Seq(result, operands) => (result.toLong, operands.trim.split(" ").map(_.toLong).toSeq)
    })
  }

}
