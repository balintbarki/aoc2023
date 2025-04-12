package aoc.aoc2024.day17

import aoc.aoc2024.DailyPuzzle2024

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case object Day17Puzzle extends DailyPuzzle2024(17, "Chronospatial Computer") {

  private val adv = 0
  private val bxl = 1
  private val bst = 2
  private val jnz = 3
  private val bxc = 4
  private val out = 5
  private val bdv = 6
  private val cdv = 7

  override def calculatePart1(lines: Seq[String]): Long = {
    val computer = getInput(lines)
    val output = computer.runProgram

    println(s"Output: ${output.mkString(",")}")
    output.mkString.toLong
  }

  override def calculatePart2(lines: Seq[String]): Long = {

    val computer = getInput(lines)
    val A_max_bits = 10
    val A_max_value = math.pow(2, A_max_bits).toInt
    val program = computer.program

    def findValues(A_values_so_far: Seq[BigInt], outputBytesCovered: Int): Seq[BigInt] = {
      if (outputBytesCovered == program.size) {
        A_values_so_far
      }
      else {
        val expectedOutput = program.takeRight(outputBytesCovered + 1)
        val potentialAs = A_values_so_far.map(_ * 8).flatMap { possibleA =>
          (0 to A_max_value).map(BigInt(_)).filter { A_lowest_3_bit =>
            val testComputer = new Computer(possibleA + A_lowest_3_bit, program)
            testComputer.runProgram == expectedOutput
          }
        }
        potentialAs
      }
    }

    val possibleAValues = findValues(Seq.empty, 0)

    possibleAValues.min.toLong
  }

  private def getInput(lines: Seq[String]): Computer = {
    val ra = BigInt(lines.head.dropWhile(_ != ':').drop(2).trim)
    val program = lines(4).dropWhile(_ != ':').drop(2).trim.split(",").map(_.toInt)

    new Computer(ra, program)
  }

  private def doubleToBigInt(value: Double): BigInt =
    BigDecimal.double2bigDecimal(value).setScale(0, BigDecimal.RoundingMode.DOWN).toBigInt

  private class Computer(val ra: BigInt, val program: Seq[Int]) {

    private val output: ListBuffer[Int] = ListBuffer.empty
    private var a: BigInt = ra
    private var b: BigInt = 0
    private var c: BigInt = 0
    private var pc = 0

    def runProgram: Seq[Int] = {
      while (pc < program.size)
        runCommand()

      output.toSeq
    }

    private def writeToOutput(value: Int): Unit = output.addOne(value)

    private def runCommand(): Unit = {

      def xdv(combo: BigInt): (BigInt, Int) = {
        val denom = math.pow(2, combo.toDouble)
        (doubleToBigInt(a.doubleValue / denom), 2)
      }

      Try {
        val opCode = program(pc)
        lazy val operand = program(pc + 1)
        lazy val combo: BigInt = operand match {
          case 0 | 1 | 2 | 3 => operand
          case 4             => a
          case 5             => b
          case 6             => c
          case unexpected    => throw new IllegalArgumentException(s"Unexpected operand: $unexpected")
        }
        lazy val literal = operand

        opCode match {
          case _ if opCode == adv =>
            val (newReg, pcShift) = xdv(combo)
            a = newReg
            pcShift

          case _ if opCode == bxl =>
            b = b ^ literal
            2

          case _ if opCode == bst =>
            b = combo % 8
            2

          case _ if opCode == jnz =>
            if (a == 0)
              1
            else {
              pc = literal
              0
            }

          case _ if opCode == bxc =>
            b = b ^ c
            2

          case _ if opCode == out =>
            writeToOutput((combo % 8).toInt)
            2

          case _ if opCode == bdv =>
            val (newReg, pcShift) = xdv(combo)
            b = newReg
            pcShift

          case _ if opCode == cdv =>
            val (newReg, pcShift) = xdv(combo)
            c = newReg
            pcShift
        }

      } match {
        case Success(pcChange) => pc += pcChange
        case Failure(_)        => pc = program.size
      }
    }
  }
}
