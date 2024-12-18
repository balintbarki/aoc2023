package aoc.aoc2023.day8

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.math.Math.lcm

import scala.annotation.tailrec

case object Day8Puzzle extends DailyPuzzle2023(8, "Haunted Wasteland") {
  override def calculatePart1(
    lines: Seq[String]): Long = {
    val (steps, desertMap) = getInput(lines)

    var nodeKey = "AAA" -> desertMap.getOrElse("AAA", ???)
    var stepCnt: Long = 0

    while (nodeKey._1 != "ZZZ") {
      val stepIndex: Int = (stepCnt % steps.size).toInt
      val step = steps(stepIndex)
      stepCnt = stepCnt + 1
      val newKey = nodeKey._2(step)

      nodeKey = newKey -> desertMap.getOrElse(newKey, throw new IllegalArgumentException("This should not happen"))
    }

    stepCnt
  }

  override def calculatePart2(
    lines: Seq[String]): Long = {
    val (steps, desertMap) = getInput(lines)

    val nodeKeys: Seq[String] = desertMap.keys.filter(_.endsWith("A")).toSeq

    val cycles = nodeKeys.map(nodeKey => {
      var currentKey = nodeKey
      var stepCnt: Long = 0
      var ZFound = 0
      var cycleCnt: Long = 0
      do {
        val stepIndex: Int = (stepCnt % steps.size).toInt
        val step = steps(stepIndex)
        stepCnt = stepCnt + 1

        val newKey = desertMap.getOrElse(currentKey, ???)(step)

        if (newKey.endsWith("Z")) {
          ZFound = ZFound + 1
          cycleCnt = stepCnt
          stepCnt = 0
        }
        currentKey = newKey
      } while (ZFound < 10)
      cycleCnt
    }).map(BigInt(_))

    lcm(cycles).toLong
  }

  private def getInput(lines: Seq[String]): (IndexedSeq[Int], Map[String, Seq[String]]) = {

    val steps = lines.head.map {
      case 'L' => 0
      case 'R' => 1
      case _   => throw new IllegalArgumentException("This should not happen")
    }

    val mapRegex = """(\w{3}) = \((\w{3}), (\w{3})\)""".r

    val desertMap = lines.tail.tail.map {
      case mapRegex(key, left, right) => key -> Seq(left, right)
    }.toMap

    (steps, desertMap)
  }
}
