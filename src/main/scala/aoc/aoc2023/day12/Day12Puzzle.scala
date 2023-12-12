package aoc.aoc2023.day12

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.Combinatory

case object Day12Puzzle extends DailyPuzzle2023(12, "unknown") {

  val operationalCode = "O"
  val damagedCode = "X"
  val unknownCode = "."

  override def calculatePart1(lines: Seq[String]): String = {

    val reducedInputs = getReducedInputs(lines)

    // Reduce operationals to 1 -> reduced input
    // Generate group strings
    // Determine number of extra operationals
    // Generate all possible combination and check if it matches with reduced input
    reducedInputs.map { case (record, groups) =>
      val minLength = groups.sum + groups.length - 1
      val recordLength = record.length
      val extraOperationalCnt: Int = recordLength - minLength
      val locationsForOperational = groups.length + 1

      // Determine each combination where "extraOperationalCnt" can be distributed to "locationsForOperation" locations
      val combinations = if (extraOperationalCnt > 0)
        Combinatory.combinations(extraOperationalCnt, locationsForOperational, 0)
          .flatMap(_.permutations)
      else
        Seq(Seq.fill(locationsForOperational)(0))

      val adjustedCombinations = combinations
        .map(combination => Seq(combination.head) ++ combination.tail.dropRight(1).map(_ + 1) ++ Seq(combination.last))

      // Check for each combination if they can match with the reduced input
      val results = adjustedCombinations.map(combination => combination.map(operationalCode * _)
        .zipAll(groups.map(damagedCode * _), "", ""))
        .map(next => next.foldLeft("") { case (prev, (first, second)) => prev + first + second })

      results.count(result => record.r.matches(result))
    }.sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = ???


  private def getReducedInputs(lines: Seq[String]) =
    lines.map(line => {
      val parts = line.split(" ")
      (parts.head.replaceAll("\\.+", operationalCode).replaceAll("#", damagedCode).replaceAll("\\?", unknownCode), parts
        .last
        .trim.split(",")
        .map(_.toInt).toSeq)
    })

  private def calculateUnfolded(lines: Seq[String], foldCnt: Int): String = {
    val reducedInputs = getReducedInputs(lines)

    // Reduce operationals to 1 -> reduced input
    // Generate group strings
    // Determine number of extra operationals
    // Generate all possible combination and check if it matches with reduced input
    reducedInputs.map { case (record, groups) =>
      val minLength = groups.sum + groups.length - 1
      val recordLength = record.length
      val extraOperationalCnt: Int = recordLength - minLength
      val locationsForOperational = groups.length + 1

      // Determine each combination where "extraOperationalCnt" can be distributed to "locationsForOperation" locations
      val combinations = if (extraOperationalCnt > 0)
        Combinatory.combinations(extraOperationalCnt, locationsForOperational, 0)
          .flatMap(_.permutations)
      else
        Seq(Seq.fill(locationsForOperational)(0))

      val adjustedCombinations = combinations
        .map(combination => Seq(combination.head) ++ combination.tail.dropRight(1).map(_ + 1) ++ Seq(combination.last))

      // Check for each combination if they can match with the reduced input
      val results = adjustedCombinations.map(combination => combination.map(operationalCode * _)
        .zipAll(groups.map(damagedCode * _), "", ""))
        .map(next => next.foldLeft("") { case (prev, (first, second)) => prev + first + second })

      results.count(result => record.r.matches(result))
    }.sum.toString
  }
}
