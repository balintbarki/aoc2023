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

  override def calculatePart2(lines: Seq[String]): Long = ???

  private def getInput(lines: Seq[String]): Computer = {
    val ra = BigInt(lines.head.dropWhile(_ != ':').drop(2).trim)
    val rb = BigInt(lines(1).dropWhile(_ != ':').drop(2).trim)
    val rc = BigInt(lines(2).dropWhile(_ != ':').drop(2).trim)
    val program = lines(4).dropWhile(_ != ':').drop(2).trim.split(",").map(_.toInt)

    new Computer(ra, rb, rc, program)
  }

  private class Computer(ra: BigInt, rb: BigInt, rc: BigInt, program: Seq[Int]) {

    private val output: ListBuffer[BigInt] = ListBuffer.empty
    private var a = ra
    private var b = rb
    private var c = rc
    private var pc = 0

    def runProgram: Seq[BigInt] = {
      while (pc < program.size)
        runCommand()

      output.toSeq
    }

    private def writeToOutput(value: BigInt) = output.addOne(value)

    private def runCommand(): Unit = {

      def xdv(combo: BigInt): (BigInt, Int) = {
        val denom = math.pow(2, combo.toDouble)
        (BigDecimal.double2bigDecimal(a.doubleValue / denom).setScale(0, BigDecimal.RoundingMode.DOWN).toBigInt, 2)
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
            writeToOutput(combo % 8)
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
