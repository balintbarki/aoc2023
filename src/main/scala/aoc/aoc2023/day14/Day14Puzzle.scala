package aoc.aoc2023.day14

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImplicitUtils.AddMultispanToSeq
import aoc.utils.ImmutableMatrix

case object Day14Puzzle extends DailyPuzzle2023(14, "Parabolic Reflector Dish") {

  override def calculatePart1(lines: Seq[String]): Long = {

    val result = tiltNorth(ImmutableMatrix.fromStrings(lines))
    calculateNorthBeamLoad(result)
  }

  override def calculatePart2(lines: Seq[String]): Long = {

    val result = runCycle(ImmutableMatrix.fromStrings(lines), 1000000000)
    calculateNorthBeamLoad(result)
  }

  def runCycle(matrix: ImmutableMatrix[Char], times: Long): ImmutableMatrix[Char] = {

    var cycle: Long = 0
    var result = matrix
    // Map containing "cached" results:
    // - Matrix hashcode
    // - Number of times it appeared
    // - Last cycle resulting this matrix
    // - Difference between the last two cycles
    var cycleMap = Map[Int, (Int, Long, Long)]()

    while (cycle < times) {
      result = tiltEast(tiltSouth(tiltWest(tiltNorth(result))))
      cycle = cycle + 1
      val hash: Int = result.hashCode()
      val (appearCntOfThis, lastCycleOfThis, _) = cycleMap.getOrElse(hash, (0, 0L, 0L))
      val cycleDiffOfThis: Long = cycle - lastCycleOfThis
      cycleMap = cycleMap + (hash -> (appearCntOfThis + 1, cycle, cycleDiffOfThis))

      if ((3 <= appearCntOfThis) && cycleMap.filter { case (_, (_, lastCycle, _)) => lastCycle >= lastCycleOfThis }
        .forall { case (_, (_, _, cycleDiff)) => cycleDiff == cycleDiffOfThis }) {

        val remainingCycles = (times - cycle) % cycleDiffOfThis
        cycle = times - remainingCycles
      }
    }

    result
  }

  private def calculateNorthBeamLoad(matrix: ImmutableMatrix[Char]): Int = {
    matrix.rows.indices.map(index => matrix.rows(index).count(_ == 'O') * (matrix.rows.length - index)).sum
  }

  private def tiltWest(matrix: ImmutableMatrix[Char]): ImmutableMatrix[Char] = {
    ImmutableMatrix.fromStrings(matrix.rows
      .map(row => row.multiSpan(_ == '#').map(_.sortWith(customLeftSort)).map(_.mkString).mkString))
  }

  private def tiltEast(matrix: ImmutableMatrix[Char]): ImmutableMatrix[Char] = {
    ImmutableMatrix.fromStrings(matrix.rows
      .map(row => row.multiSpan(_ == '#').map(_.sortWith(customRightSort)).map(_.mkString).mkString))
  }

  private def tiltNorth(matrix: ImmutableMatrix[Char]): ImmutableMatrix[Char] = {
    tiltWest(matrix.transpose).transpose
  }

  private def tiltSouth(matrix: ImmutableMatrix[Char]): ImmutableMatrix[Char] = {
    tiltEast(matrix.transpose).transpose
  }

  private def customLeftSort(a: Char, b: Char): Boolean = {
    val priorities = Map(
      '#' -> 0,
      'O' -> 1,
      '.' -> 2
    )

    priorities.getOrElse(a, ???) < priorities.getOrElse(b, ???)
  }

  private def customRightSort(a: Char, b: Char): Boolean = {
    val priorities = Map(
      '#' -> 0,
      '.' -> 1,
      'O' -> 2,
    )

    priorities.getOrElse(a, ???) < priorities.getOrElse(b, ???)
  }
}
